{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Application
    ( backendMain
    , awaitAndProcessJob
    ) where

import Import hiding (runDB)

import Backend.DB
import Backend.Foundation
import Backend.Job
import Backend.Metrics
import Control.Monad ((<=<))
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Database.Redis (checkedConnect)
import GitHub.Data.AccessTokens
import GitHub.Endpoints.Installations
import LoadEnv (loadEnv)
import Model.AppMetrics
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..))
import System.Process (readProcessWithExitCode)

backendMain :: IO ()
backendMain = do
    loadEnv
    backendSettings <- loadEnvSettings

    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    -- In the backend, we just log to stdout; so it's simpler to repeat that
    -- knowledge here than to do the chicken-and-egg dance as in the
    -- construction of the Application connection pool.
    backendConnPool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf backendSettings)
        (pgPoolSize $ appDatabaseConf backendSettings)

    backendRedisConn <- checkedConnect (appRedisConf backendSettings)
    backendMetrics <- buildAppMetrics

    let store = amStore backendMetrics
    if appCloudWatchEKG backendSettings
        then forkCloudWatchServer store
        else forkLocalhostServer store 8001

    runBackend Backend {..} $ forever $ awaitAndProcessJob 120

awaitAndProcessJob :: MonadBackend m => Integer -> m ()
awaitAndProcessJob = traverse_ processJob <=< awaitRestylerJob

processJob :: MonadBackend m => Entity Job -> m ()
processJob job = do
    logInfoN
        $ "Processing Restyler Job Id "
        <> toPathPiece (entityKey job)
        <> ": "
        <> tshow (entityVal job)
    settings <- asks backendSettings
    (ec, out, err) <- execRestyler settings job `catchAny` \ex -> do
            -- Log and act like a failed process
        logErrorN $ tshow ex
        pure (ExitFailure 1, "", show ex)


    recordJobMetrics ec
    runDB $ completeJob (entityKey job) ec (pack out) (pack err)
  where
    recordJobMetrics ExitSuccess = jobAttempted >> jobSucceeded
    recordJobMetrics _ = jobAttempted >> jobFailed

execRestyler
    :: MonadBackend m
    => AppSettings
    -> Entity Job
    -> m (ExitCode, String, String)
execRestyler appSettings@AppSettings {..} (Entity jobId Job {..}) = do
    repo <- fromMaybeM (throwString "Repo not found")
        =<< runDB (getBy $ UniqueRepo jobOwner jobRepo)

    when (repoIsPrivate $ entityVal repo) $ runDB $ do
        let
            err = throwString $ unpack $ unlines
                [ "No active plan for private repository: "
                <> toPathPiece (repoOwner $ entityVal repo)
                <> "/"
                <> toPathPiece (repoName $ entityVal repo)
                , ""
                , "Contact support@restyled.io if you would like to discuss a Trial"
                ]

        now <- liftIO getCurrentTime
        void . fromMaybeM err =<< selectActivePlan now (entityVal repo)

    eAccessToken <- liftIO
        $ createAccessToken appGitHubAppId appGitHubAppKey jobInstallationId

    either
        throwString
        (\AccessToken {..} -> readLoggedProcess
            "docker"
            [ "run"
            , "--rm"
            , "--env"
            , debugEnv repo
            , "--env"
            , "GITHUB_ACCESS_TOKEN=" <> unpack atToken
            , "--volume"
            , "/tmp:/tmp"
            , "--volume"
            , "/var/run/docker.sock:/var/run/docker.sock"
            , appRestylerImage ++ maybe "" (":" ++) appRestylerTag
            , "--job-url"
            , unpack
            $ appRoot
            <> "/gh/"
            <> toPathPiece jobOwner
            <> "/repos/"
            <> toPathPiece jobRepo
            <> "/jobs/"
            <> toPathPiece jobId
            , unpack
            $ toPathPiece jobOwner
            <> "/"
            <> toPathPiece jobRepo
            <> "#"
            <> toPathPiece jobPullRequest
            ]
        )
        eAccessToken
  where
    debugEnv (Entity _ Repo {..})
        | appSettings `allowsLevel` LevelDebug = "DEBUG=1"
        | repoDebugEnabled = "DEBUG=1"
        | otherwise = "DEBUG="

readLoggedProcess
    :: (MonadIO m, MonadLogger m)
    => String
    -> [String]
    -> m (ExitCode, String, String)
readLoggedProcess cmd args = do
    logDebugN $ "process: " <> tshow (cmd : args)
    result <- liftIO $ readProcessWithExitCode cmd args ""
    logDebugN $ "process result: " <> tshow result
    pure result

selectActivePlan
    :: MonadIO m => UTCTime -> Repo -> SqlPersistT m (Maybe (Entity Plan))
selectActivePlan now repo = selectFirst filters [Desc PlanId]
  where
    filters = repoFilters <> activeFilters <> expiredFilters
    repoFilters = [PlanOwner ==. repoOwner repo, PlanRepo ==. repoName repo]
    activeFilters = [PlanActiveAt ==. Nothing] ||. [PlanActiveAt <=. Just now]
    expiredFilters =
        [PlanExpiresAt ==. Nothing] ||. [PlanExpiresAt >=. Just now]

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
import Control.Monad ((<=<))
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Database.Redis (checkedConnect)
import LoadEnv (loadEnv)
import SVCS.GitHub.AccessToken (githubInstallationToken)
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

    backendConnPool <- runBackendLogger backendSettings $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf backendSettings)
        (pgPoolSize $ appDatabaseConf backendSettings)

    backendRedisConn <- checkedConnect (appRedisConf backendSettings)

    runBackend Backend {..} $ forever $ awaitAndProcessJob 120

awaitAndProcessJob :: MonadBackend m => Integer -> m ()
awaitAndProcessJob = traverse_ processJob <=< awaitRestylerJob

-- brittany-next-binding --columns=85

processJob :: MonadBackend m => Entity Job -> m ()
processJob job@(Entity jobId Job {..}) = do
    logInfoN
        $ "Processing Restyler Job Id "
        <> toPathPiece jobId
        <> ": "
        <> repoPullPath jobOwner jobRepo jobPullRequest

    result <- runDB $ guardRepositoryJob job

    (ec, out, err) <- case result of
        RepoNotFound -> jobSkipped "Repo not found"
        NonGitHub repo -> jobSkipped $ nonGitHubMsg $ entityVal repo
        PrivateNoPlan repo -> jobSkipped $ privateNoPlanMsg $ entityVal repo
        JobCanProceed repo -> handleAny (jobFailed . show) $ execRestyler repo job

    runDB $ completeJob jobId ec (pack out) (pack err)

jobSkipped :: Applicative f => String -> f (ExitCode, String, String)
jobSkipped msg = pure (ExitSuccess, "", "Job skipped: " <> msg)

jobFailed :: MonadLogger m => String -> m (ExitCode, String, String)
jobFailed msg = do
    logErrorN $ pack msg
    pure (ExitFailure 1, "", msg)

data JobGuardResult
    = RepoNotFound
    | NonGitHub (Entity Repo)
    | PrivateNoPlan (Entity Repo)
    | JobCanProceed (Entity Repo)

guardRepositoryJob :: MonadIO m => Entity Job -> SqlPersistT m JobGuardResult
guardRepositoryJob (Entity _ Job {..}) = do
    mRepo <- getBy $ UniqueRepo jobSvcs jobOwner jobRepo

    case mRepo of
        Nothing -> pure RepoNotFound
        Just repo@(Entity _ Repo {..})
            | repoSvcs /= GitHubSVCS -> pure $ NonGitHub repo
            | repoIsPrivate -> do
                now <- liftIO getCurrentTime
                mPlan <- selectActivePlan now $ entityVal repo
                pure $ maybe
                    (PrivateNoPlan repo)
                    (const $ JobCanProceed repo)
                    mPlan
            | otherwise -> pure $ JobCanProceed repo

-- brittany-disable-next-binding

execRestyler
    :: MonadBackend m
    => Entity Repo
    -> Entity Job
    -> m (ExitCode, String, String)
execRestyler (Entity _ Repo {..}) (Entity jobId Job {..}) = do
    appSettings@AppSettings {..} <- asks backendSettings

    let debugEnv
            | appSettings `allowsLevel` LevelDebug = "DEBUG=1"
            | repoDebugEnabled = "DEBUG=1"
            | otherwise = "DEBUG="

        jobUrl = appRoot
            <> "/gh/" <> toPathPiece jobOwner
            <> "/repos/" <> toPathPiece jobRepo
            <> "/jobs/" <> toPathPiece jobId

    eAccessToken <- liftIO $ githubInstallationToken
        appGitHubAppId
        appGitHubAppKey
        repoInstallationId

    either
        throwString
        (\token -> readLoggedProcess
            "docker"
            [ "run" , "--rm"
            , "--env" , debugEnv
            , "--env" , "GITHUB_ACCESS_TOKEN=" <> unpack (unRepoAccessToken token)
            , "--volume" , "/tmp:/tmp"
            , "--volume" , "/var/run/docker.sock:/var/run/docker.sock"
            , appRestylerImage ++ maybe "" (":" ++) appRestylerTag
            , "--job-url" , unpack jobUrl
            , unpack $ repoPullPath jobOwner jobRepo jobPullRequest
            ]
        )
        eAccessToken

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
selectActivePlan now repo = selectFirst
    (concat
        [ [PlanOwner ==. repoOwner repo, PlanRepo ==. repoName repo]
        , [PlanActiveAt ==. Nothing] ||. [PlanActiveAt <=. Just now]
        , [PlanExpiresAt ==. Nothing] ||. [PlanExpiresAt >=. Just now]
        ]
    )
    []

nonGitHubMsg :: Repo -> String
nonGitHubMsg Repo {..} = unpack $ unlines
    [ "Non-GitHub (" <> tshow repoSvcs <> "): " <> path <> "."
    , "See https://github.com/restyled-io/restyled.io/issues/76"
    ]
    where path = repoPath repoOwner repoName

privateNoPlanMsg :: Repo -> String
privateNoPlanMsg Repo {..} = unpack $ unlines
    [ "No active plan for private repository: " <> path <> "."
    , "Contact support@restyled.io if you would like to discuss a Trial"
    ]
    where path = repoPath repoOwner repoName

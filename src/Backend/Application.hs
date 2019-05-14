{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Application
    ( backendMain
    , awaitAndProcessJob
    ) where

import Import hiding (runDB)

import Backend.DB
import Backend.ExecRestyler
import Backend.Foundation
import Backend.Job
import Backend.Marketplace
import Backend.Webhook
import Control.Monad ((<=<))
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Database.Redis (checkedConnect)
import Model.RestyleMachine (runRestyleMachine)
import SVCS.GitHub.AccessToken (githubInstallationToken)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..))

backendMain :: IO ()
backendMain = do
    backendSettings <- loadEnvSettings

    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    backendConnPool <- runBackendLogger backendSettings $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf backendSettings)
        (pgPoolSize $ appDatabaseConf backendSettings)

    backendRedisConn <- checkedConnect (appRedisConf backendSettings)

    runBackend Backend {..} $ do
        -- LEGACY: flush old pre-processed Jobs queue
        void $ async $ forever $ awaitAndProcessJob 120
        void $ async synchronizeMarketplacePlans
        forever $ awaitAndProcessWebhook 120

awaitAndProcessJob :: MonadBackend m => Integer -> m ()
awaitAndProcessJob = traverse_ processJob <=< awaitRestylerJob

awaitAndProcessWebhook :: MonadBackend m => Integer -> m ()
awaitAndProcessWebhook = traverse_ processWebhook' <=< awaitWebhook
    where processWebhook' = processWebhook $ ExecRestyler execRestyler

-- brittany-next-binding --columns=85

processJob :: MonadBackend m => Entity Job -> m ()
processJob job@(Entity jobId Job {..}) = do
    logInfoN
        $ "Processing Restyler Job Id "
        <> toPathPiece jobId
        <> ": "
        <> repoPullPath jobOwner jobRepo jobPullRequest

    result <- runDB $ guardRepositoryJob job
    processResult <- case result of
        RepoNotFound -> jobSkipped "Repo not found"
        NonGitHub repo -> jobSkipped $ nonGitHubMsg repo
        PlanChecked (MarketplacePlanForbids limitation) repo ->
            jobSkipped $ planLimitation limitation repo
        PlanChecked MarketplacePlanAllows repo ->
            handleAny (jobFailed . show) $ execRestyler repo job

    now <- liftIO getCurrentTime
    runDB $ replace jobId $ completeJob now processResult $ entityVal job

jobSkipped :: Applicative f => String -> f (ExitCode, String, String)
jobSkipped msg = pure (ExitSuccess, "", "Job skipped: " <> msg)

jobFailed :: MonadLogger m => String -> m (ExitCode, String, String)
jobFailed msg = do
    logErrorN $ pack msg
    pure (ExitFailure 1, "", msg)

data JobGuardResult
    = RepoNotFound
    | NonGitHub (Entity Repo)
    | PlanChecked MarketplacePlanAllows (Entity Repo)

guardRepositoryJob :: MonadIO m => Entity Job -> SqlPersistT m JobGuardResult
guardRepositoryJob (Entity _ Job {..}) = do
    mRepo <- getBy $ UniqueRepo jobSvcs jobOwner jobRepo
    maybe (pure RepoNotFound) checkRepo mRepo
  where
    checkRepo repo
        | isNonGitHub repo = pure $ NonGitHub repo
        | otherwise = checkPlan repo

    checkPlan repo = PlanChecked <$> marketplacePlanAllows repo <*> pure repo

isNonGitHub :: Entity Repo -> Bool
isNonGitHub = (/= GitHubSVCS) . repoSvcs . entityVal

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

    machines <- runDB
        $ entityVal <$$> selectList [RestyleMachineEnabled ==. True] []

    either
        throwString
        (\token -> runRestyleMachine machines "docker"
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

nonGitHubMsg :: Entity Repo -> String
nonGitHubMsg (Entity _ Repo {..}) = unpack $ unlines
    [ "Non-GitHub (" <> tshow repoSvcs <> "): " <> path <> "."
    , "See https://github.com/restyled-io/restyled.io/issues/76"
    ]
    where path = repoPath repoOwner repoName

planLimitation :: MarketplacePlanLimitation -> Entity Repo -> String
planLimitation MarketplacePlanNotFound (Entity _ Repo {..}) = unpack $ unlines
    [ "No active plan for private repository: " <> path <> "."
    , "Contact support@restyled.io if you would like to discuss a Trial"
    ]
    where path = repoPath repoOwner repoName

planLimitation MarketplacePlanPublicOnly _ = unpack $ unlines @Text
    [ "Your plan does not allow private repositories."
    , "Contact support@restyled.io if you would like to discuss a Trial"
    ]

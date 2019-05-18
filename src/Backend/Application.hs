module Backend.Application
    ( backendMain
    , awaitAndProcessJob
    ) where

import Backend.Import

import Backend.ExecRestyler
import Backend.Foundation
import Backend.Job
import Backend.Marketplace
import Backend.Webhook
import Control.Monad ((<=<))
import Model.RestyleMachine (runRestyleMachine)
import RIO.Process
import RIO.Process.Follow
import SVCS.GitHub.AccessToken (githubInstallationToken)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..))

backendMain :: IO ()
backendMain = do
    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    backend <- loadBackend

    runRIO backend $ do
        -- LEGACY: flush old pre-processed Jobs queue
        void $ async $ forever $ awaitAndProcessJob 120
        void $ async synchronizeMarketplacePlans
        forever $ awaitAndProcessWebhook 120

awaitAndProcessJob
    :: ( HasLogFunc env
       , HasProcessContext env
       , HasSettings env
       , HasDB env
       , HasRedis env
       )
    => Integer
    -> RIO env ()
awaitAndProcessJob = traverse_ processJob <=< awaitRestylerJob

awaitAndProcessWebhook
    :: ( HasLogFunc env
       , HasProcessContext env
       , HasSettings env
       , HasDB env
       , HasRedis env
       )
    => Integer
    -> RIO env ()
awaitAndProcessWebhook = traverse_ processWebhook' <=< awaitWebhook
    where processWebhook' = processWebhook $ ExecRestyler execRestyler

processJob
    :: (HasLogFunc env, HasProcessContext env, HasSettings env, HasDB env)
    => Entity Job
    -> RIO env ()
processJob job@(Entity jobId Job {..}) = do
    logInfo
        $ fromString
        $ unpack
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

jobFailed :: HasLogFunc env => String -> RIO env (ExitCode, String, String)
jobFailed msg = do
    logError $ fromString msg
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
    :: (HasLogFunc env, HasProcessContext env, HasSettings env, HasDB env)
    => Entity Repo
    -> Entity Job
    -> RIO env (ExitCode, String, String)
execRestyler (Entity _ Repo {..}) (Entity jobId Job {..}) = do
    appSettings@AppSettings {..} <- view settingsL

    let debugEnv
            | appSettingsIsDebug appSettings = "DEBUG=1"
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
        (\token ->
            captureFollowedProcessWith
                (captureJobLogLine jobId "stdout" . pack)
                (captureJobLogLine jobId "stderr" . pack)
                $ runRestyleMachine machines "docker"
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

captureJobLogLine :: HasDB env => JobId -> Text -> Text -> RIO env ()
captureJobLogLine _jobId _stream _content = runDB $ pure () -- do
    -- now <- liftIO getCurrentTime
    -- insert_ JobLogLine
    --     { jobLogLineJob = jobId
    --     , jobLogLineCreatedAt = now
    --     , jobLogLineStream = stream
    --     , jobLogLineContent = content
    --     }

nonGitHubMsg :: Entity Repo -> String
nonGitHubMsg (Entity _ Repo {..}) = unlines
    [ "Non-GitHub (" <> show repoSvcs <> "): " <> path <> "."
    , "See https://github.com/restyled-io/restyled.io/issues/76"
    ]
    where path = unpack $ repoPath repoOwner repoName

planLimitation :: MarketplacePlanLimitation -> Entity Repo -> String
planLimitation MarketplacePlanNotFound (Entity _ Repo {..}) = unlines
    [ "No active plan for private repository: " <> path <> "."
    , "Contact support@restyled.io if you would like to discuss a Trial"
    ]
    where path = unpack $ repoPath repoOwner repoName

planLimitation MarketplacePlanPublicOnly _ = unlines
    [ "Your plan does not allow private repositories."
    , "Contact support@restyled.io if you would like to discuss a Trial"
    ]

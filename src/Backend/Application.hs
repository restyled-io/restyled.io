module Backend.Application
    ( backendMain
    , awaitAndProcessJob
    ) where

import Backend.Import

import Backend.ExecRestyler
import Backend.Foundation
import Backend.Job
import Backend.Marketplace
import Backend.RestyleMachine
import Backend.Webhook
import Control.Monad ((<=<))
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
        void $ async synchronizeMarketplacePlans
        void $ async $ forever $ awaitAndProcessJob 120
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
awaitAndProcessJob = traverse_ processJob' <=< awaitRestylerJob
    where processJob' = processJob $ ExecRestyler execRestyler

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

execRestyler
    :: (HasLogFunc env, HasProcessContext env, HasSettings env, HasDB env)
    => Entity Repo
    -> Entity Job
    -> RIO env (ExitCode, String, String)
execRestyler (Entity _ repo) job = do
    settings <- view settingsL
    token <- fromLeftM throwString $ liftIO $ githubInstallationToken
        (appGitHubAppId settings)
        (appGitHubAppKey settings)
        (repoInstallationId repo)

    let debug = appSettingsIsDebug settings || repoDebugEnabled repo

    machines <-
        runDB $ entityVal <$$> selectList [RestyleMachineEnabled ==. True] []

    captureFollowedProcessWith
            (captureJobLogLine (entityKey job) "stdout" . pack)
            (captureJobLogLine (entityKey job) "stderr" . pack)
        $ runRestyleMachine machines "docker"
        $ restyleDockerRun settings token job debug

-- brittany-disable-next-binding

restyleDockerRun
    :: AppSettings
    -> RepoAccessToken
    -> Entity Job
    -> Bool -- ^ Debug?
    -> [String]
restyleDockerRun AppSettings {..} token (Entity jobId Job {..}) debug =
    [ "run", "--rm"
    , "--env", if debug then "DEBUG=1" else "DEBUG="
    , "--env", "GITHUB_ACCESS_TOKEN=" <> unpack (unRepoAccessToken token)
    , "--volume", "/tmp:/tmp"
    , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
    , restylerImage, "--job-url", jobUrl, prSpec
    ]
  where
    restylerImage = appRestylerImage <> maybe "" (":" <>) appRestylerTag

    jobUrl =
        unpack
            $ appRoot
            <> "/gh/"
            <> toPathPiece jobOwner
            <> "/repos/"
            <> toPathPiece jobRepo
            <> "/jobs/"
            <> toPathPiece jobId

    prSpec = unpack $ repoPullPath jobOwner jobRepo jobPullRequest

captureJobLogLine :: HasDB env => JobId -> Text -> Text -> RIO env ()
captureJobLogLine jobId stream content = runDB $ do
    now <- liftIO getCurrentTime
    insert_ JobLogLine
        { jobLogLineJob = jobId
        , jobLogLineCreatedAt = now
        , jobLogLineStream = stream
        , jobLogLineContent = content
        }

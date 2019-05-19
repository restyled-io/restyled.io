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

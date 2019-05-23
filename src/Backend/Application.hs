{-# LANGUAGE LambdaCase #-}

module Backend.Application
    ( backendMain
    )
where

import Backend.Import

import Backend.ExecRestyler
import Backend.Foundation
import Backend.Job
import Backend.Marketplace
import Backend.RestyleMachine
import Backend.Webhook

backendMain :: IO ()
backendMain = do
    setLineBuffering

    loadEnv
    backend <- loadBackend =<< loadEnvSettings

    runRIO backend $ do
        asyncs <- sequence
            [ async synchronizeMarketplacePlans
            , async $ runQueue (awaitJob 120) $ processJob execRestyler
            , async $ runQueue (awaitWebhook 120) $ processWebhook execRestyler
            ]

        void $ waitAny asyncs

runQueue :: Monad m => m (Maybe a) -> (a -> m ()) -> m b
runQueue awaitItem processItem = forever $ traverse_ processItem =<< awaitItem

execRestyler
    :: (HasLogFunc env, HasProcessContext env, HasSettings env, HasDB env)
    => ExecRestyler (RIO env)
execRestyler = ExecRestyler $ \(Entity _ repo) job -> do
    settings <- view settingsL
    token <- fromLeftM throwString $ liftIO $ githubInstallationToken
        (appGitHubAppId settings)
        (appGitHubAppKey settings)
        (repoInstallationId repo)

    let capture stream = captureJobLogLine (entityKey job) stream . pack

    runRestyle <- runDB $ do
        mMachine <- fetchRestyleMachine

        case mMachine of
            Nothing -> do
                capture "system" "Running on local Docker host"
                pure followProcess
            Just machine -> do
                capture "system" $ "Running on " <> displayMachine machine
                pure $ runRestyleMachine machine

    ec <- runRestyle
        "docker"
        (restyleDockerRun settings token job $ repoIsDebug settings repo)
        (runDB . capture "stdout")
        (runDB . capture "stderr")

    ec <$ runDB (capture "system" $ "Restyler exited " <> displayExitCode ec)

displayExitCode :: ExitCode -> String
displayExitCode = \case
    ExitSuccess -> "0"
    ExitFailure c -> show c

displayMachine :: RestyleMachine -> String
displayMachine = unpack . restyleMachineHost

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

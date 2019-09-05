{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Application
    ( runBackend
    )
where

import Restyled.Prelude

import Restyled.Backend.DockerRun
import Restyled.Backend.ExecRestyler
import Restyled.Backend.Job
import Restyled.Backend.Marketplace
import Restyled.Backend.RestyleMachine
import Restyled.Backend.Webhook
import Restyled.Models
import Restyled.Settings

runBackend
    :: ( HasCallStack
       , HasLogFunc env
       , HasSettings env
       , HasDB env
       , HasRedis env
       , HasProcessContext env
       )
    => RIO env ()
runBackend = do
    asyncs <- sequence
        [ async synchronizeMarketplacePlans
        , async $ runQueue (awaitJob 120) $ processJob execRestyler
        , async $ runQueue (awaitWebhook 120) $ processWebhook execRestyler
        ]

    void $ waitAny asyncs

runQueue :: Monad m => m (Maybe a) -> (a -> m ()) -> m b
runQueue awaitItem processItem = forever $ traverse_ processItem =<< awaitItem

execRestyler
    :: ( HasCallStack
       , HasLogFunc env
       , HasProcessContext env
       , HasSettings env
       , HasDB env
       )
    => ExecRestyler (RIO env)
execRestyler = ExecRestyler $ \(Entity _ repo) job -> do
    settings <- view settingsL
    token <- repoInstallationToken settings repo

    let capture stream = captureJobLogLine (entityKey job) stream . pack

    runRestyle <- runDB $ do
        capture "system" $ unwords $ "docker" : dockerRunArgsLogged settings job

        fetchRestyleMachine >>= \case
            Nothing -> do
                capture "system" "Running on local Docker host"
                pure followProcess
            Just machine -> do
                capture "system" $ "Running on " <> displayMachine machine
                pure $ runRestyleMachine machine

    ec <- runRestyle
        "docker"
        (dockerRunArgs settings token repo job)
        (runDB . capture "stdout")
        (runDB . capture "stderr")

    ec <$ runDB (capture "system" $ "Restyler exited " <> displayExitCode ec)

repoInstallationToken
    :: (HasCallStack, MonadIO m) => AppSettings -> Repo -> m AccessToken
repoInstallationToken AppSettings {..} Repo {..} = do
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    untryIO $ accessTokenFor auth repoInstallationId

displayExitCode :: ExitCode -> String
displayExitCode = \case
    ExitSuccess -> "0"
    ExitFailure c -> show c

displayMachine :: RestyleMachine -> String
displayMachine = unpack . restyleMachineHost

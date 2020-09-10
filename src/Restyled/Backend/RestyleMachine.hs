module Restyled.Backend.RestyleMachine
    ( withRestyleMachine
    , withRestyleMachineEnv
    , deleteRestyleMachine

    -- * Exported for testing
    , withExtraEnvVars
    )
where

import Restyled.Prelude

import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Restyled.Models
import Restyled.Settings
import RIO.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getHomeDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))

-- | Fetch a Machine, and increment its @jobCount@ during execution
--
-- Won't ever select an overloaded machine (appRestyleMachineJobsMax), and
-- instead blocks until one becomes available.
--
-- Risk: if we get into an issue where @restyle_machines.job_count@ is not being
-- managed correctly (and so never decrementing), we might have a no machines
-- available even if they're all empty.
--
withRestyleMachine
    :: (HasSettings env, HasDB env, HasLogFunc env)
    => (Entity RestyleMachine -> RIO env a)
    -> RIO env a
withRestyleMachine f = do
    jobsMax <- appRestyleMachineJobsMax <$> view settingsL
    machine <- throttleWarn $ runDB $ do
        mMachine <- selectFirst
            [ RestyleMachineEnabled ==. True
            , RestyleMachineJobCount <. fromIntegral jobsMax
            ]
            [Asc RestyleMachineJobCount]
        mMachine <$ traverse_ increment mMachine
    f machine `finally` runDB (decrement machine)
  where
    increment :: MonadIO m => Entity RestyleMachine -> SqlPersistT m ()
    increment = flip update [RestyleMachineJobCount +=. 1] . entityKey

    decrement :: MonadIO m => Entity RestyleMachine -> SqlPersistT m ()
    decrement = flip update [RestyleMachineJobCount -=. 1] . entityKey

throttleWarn :: HasLogFunc env => RIO env (Maybe a) -> RIO env a
throttleWarn act = do
    mVal <- act
    case mVal of
        Nothing -> do
            logError "No Restyle Machine available, sleeping 1m"
            threadDelay $ 60 * 1000000
            throttleWarn act
        Just val -> pure val

-- | Run a process on a @'RestyleMachine'@s
withRestyleMachineEnv
    :: (MonadIO m, HasProcessContext env, MonadReader env m)
    => RestyleMachine
    -> m a
    -> m a
withRestyleMachineEnv machine f = do
    certPath <- restyleMachineCertPath machine
    setupCertificatesIfMissing machine certPath

    withExtraEnvVars
        [ ("DOCKER_HOST", restyleMachineHost machine)
        , ("DOCKER_CERT_PATH", pack certPath)
        , ("DOCKER_TLS_VERIFY", "1")
        ]
        f

deleteRestyleMachine :: MonadIO m => Entity RestyleMachine -> SqlPersistT m ()
deleteRestyleMachine (Entity machineId machine) = do
    delete machineId
    certPath <- restyleMachineCertPath machine
    whenM (doesDirectoryExist certPath) $ removeDirectoryRecursive certPath

withExtraEnvVars
    :: (HasProcessContext env, MonadReader env m, MonadIO m)
    => [(Text, Text)]
    -> m a
    -> m a
withExtraEnvVars = withModifyEnvVars . Map.union . Map.fromList

setupCertificatesIfMissing :: MonadIO m => RestyleMachine -> FilePath -> m ()
setupCertificatesIfMissing RestyleMachine {..} certPath =
    unlessM (doesDirectoryExist certPath) $ do
        createDirectoryIfMissing True certPath
        liftIO $ do
            T.writeFile (certPath </> "ca.pem") restyleMachineCaCert
            T.writeFile (certPath </> "cert.pem") restyleMachineCert
            T.writeFile (certPath </> "key.pem") restyleMachineKey

restyleMachineCertPath :: MonadIO m => RestyleMachine -> m FilePath
restyleMachineCertPath RestyleMachine {..} = do
    home <- getHomeDirectory
    pure $ home </> ".docker" </> "restyled" </> "cert" </> name
    where name = unpack restyleMachineName

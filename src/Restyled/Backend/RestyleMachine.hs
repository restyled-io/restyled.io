{-# LANGUAGE DerivingVia #-}

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
import RIO.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getHomeDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))

-- | Fetch a Machine, and increment its @jobCount@ during execution
withRestyleMachine
    :: HasDB env => (Maybe (Entity RestyleMachine) -> RIO env a) -> RIO env a
withRestyleMachine f = do
    mMachine <- runDB $ do
        mMachine <- selectFirst
            [RestyleMachineEnabled ==. True]
            [Asc RestyleMachineJobCount]
        mMachine <$ traverse_ increment mMachine
    f mMachine `finally` runDB (traverse_ decrement mMachine)
  where
    increment = flip update [RestyleMachineJobCount +=. 1] . entityKey
    decrement = flip update [RestyleMachineJobCount -=. 1] . entityKey

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

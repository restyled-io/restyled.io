{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Model.RestyleMachine
    ( runRestyleMachine
    )
where

import Prelude

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as T
import Model
import System.Directory
    (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)

-- | Run a process on one of the given @'RestyleMachine'@s
--
-- If the list is empty, the process is run wherever the global environment
-- variables (e.g. @DOCKER_HOST@) dictate.
--
runRestyleMachine
    :: (MonadIO m, MonadLogger m)
    => [RestyleMachine]
    -> FilePath
    -> [String]
    -> m (ExitCode, String, String)
runRestyleMachine machines cmd args = do
    run <- case machines of
        [] -> do
            logInfoN "Running restyle with native environment"
            pure $ runProcess Nothing

        (machine@RestyleMachine {..} : _) -> do
            logInfoN
                $ "Running restyle with environment for "
                <> restyleMachineName
                <> " ("
                <> restyleMachineHost
                <> ")"

            certPath <- setupCertificatesIfMissing machine
            pure $ runProcess $ Just
                [ ("DOCKER_HOST", unpack restyleMachineHost)
                , ("DOCKER_CERT_PATH", certPath)
                , ("DOCKER_TLS_VERIFY", "1")
                ]

    run cmd args

runProcess
    :: (MonadLogger m, MonadIO m)
    => Maybe [(String, String)]
    -> String
    -> [String]
    -> m (ExitCode, String, String)
runProcess env' cmd args = do
    logDebugN $ "process: " <> tshow (cmd : args)
    logDebugN $ "environment: " <> tshow env'
    result <- liftIO
        $ readCreateProcessWithExitCode (proc cmd args) { env = env' } ""
    logDebugN $ "process result: " <> tshow result
    pure result

setupCertificatesIfMissing
    :: (MonadIO m, MonadLogger m) => RestyleMachine -> m FilePath
setupCertificatesIfMissing RestyleMachine {..} = do
    (certPath, certPathExists) <- liftIO $ do
        home <- getHomeDirectory
        let
            path =
                home
                    </> ".docker"
                    </> "restyled"
                    </> "cert"
                    </> unpack restyleMachineName
        (path, ) <$> doesDirectoryExist path

    unless certPathExists $ do
        logInfoN $ "Populating certificates in " <> pack certPath
        liftIO $ do
            createDirectoryIfMissing True certPath
            T.writeFile (certPath </> "ca.pem") restyleMachineCaCert
            T.writeFile (certPath </> "cert.pem") restyleMachineCert
            T.writeFile (certPath </> "key.pem") restyleMachineKey

    pure certPath

tshow :: Show a => a -> Text
tshow = pack . show

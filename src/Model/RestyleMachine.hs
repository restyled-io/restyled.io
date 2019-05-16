-- | FIXME: this should be in Backend, not Model
module Model.RestyleMachine
    ( runRestyleMachine
    )
where

import RIO

import Data.Text (unpack)
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
    :: HasLogFunc env
    => [RestyleMachine]
    -> FilePath
    -> [String]
    -> RIO env (ExitCode, String, String)
runRestyleMachine machines cmd args = do
    run <- case machines of
        [] -> do
            logInfo "Running restyle with native environment"
            pure $ runProcess Nothing

        (machine@RestyleMachine {..} : _) -> do
            logInfo
                $ fromString
                $ "Running restyle with environment for "
                <> unpack restyleMachineName
                <> " ("
                <> unpack restyleMachineHost
                <> ")"

            certPath <- setupCertificatesIfMissing machine
            pure $ runProcess $ Just
                [ ("DOCKER_HOST", unpack restyleMachineHost)
                , ("DOCKER_CERT_PATH", certPath)
                , ("DOCKER_TLS_VERIFY", "1")
                ]

    run cmd args

runProcess
    :: HasLogFunc env
    => Maybe [(String, String)]
    -> String
    -> [String]
    -> RIO env (ExitCode, String, String)
runProcess env' cmd args = do
    logDebug $ "process: " <> displayShow (cmd : args)
    logDebug $ "environment: " <> displayShow env'
    result <- liftIO
        $ readCreateProcessWithExitCode (proc cmd args) { env = env' } ""
    logDebug $ "process result: " <> displayShow result
    pure result

setupCertificatesIfMissing
    :: HasLogFunc env => RestyleMachine -> RIO env FilePath
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
        logInfo $ fromString $ "Populating certificates in " <> certPath
        liftIO $ do
            createDirectoryIfMissing True certPath
            T.writeFile (certPath </> "ca.pem") restyleMachineCaCert
            T.writeFile (certPath </> "cert.pem") restyleMachineCert
            T.writeFile (certPath </> "key.pem") restyleMachineKey

    pure certPath

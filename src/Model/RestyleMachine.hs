-- | FIXME: this should be in Backend, not Model
module Model.RestyleMachine
    ( runRestyleMachine
    )
where

import RIO

import qualified Data.Map as Map
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T
import Model
import RIO.Process
import RIO.Process.Follow
import System.Directory
    (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))

-- | Run a process on one of the given @'RestyleMachine'@s
--
-- If the list is empty, the process is run wherever the global environment
-- variables (e.g. @DOCKER_HOST@) dictate.
--
runRestyleMachine
    :: (HasLogFunc env, HasProcessContext env)
    => [RestyleMachine]
    -> FilePath
    -> [String]
    -> (String -> RIO env ())
    -> (String -> RIO env ())
    -> RIO env ExitCode
runRestyleMachine machines cmd args fOut fErr = do
    modEnvVars <- case machines of
        [] -> do
            logInfo "Running restyle with native environment"
            pure id

        (machine@RestyleMachine {..} : _) -> do
            logInfo
                $ fromString
                $ "Running restyle with environment for "
                <> unpack restyleMachineName
                <> " ("
                <> unpack restyleMachineHost
                <> ")"

            certPath <- setupCertificatesIfMissing machine
            pure $ \env -> env <> Map.fromList
                [ ("DOCKER_HOST", restyleMachineHost)
                , ("DOCKER_CERT_PATH", pack certPath)
                , ("DOCKER_TLS_VERIFY", "1")
                ]

    withModifyEnvVars modEnvVars $ followProcess cmd args fOut fErr

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

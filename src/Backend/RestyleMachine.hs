{-# LANGUAGE QuasiQuotes #-}

module Backend.RestyleMachine
    ( fetchRestyleMachine
    , runRestyleMachine
    )
where

import Backend.Import

import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Database.Persist.Sql (rawSql)
import System.Directory
    (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import Text.Shakespeare.Text (st)

-- brittany-disable-next-binding

-- | Select a @'RestyleMachine'@ from the enabled set
fetchRestyleMachine :: MonadIO m => SqlPersistT m (Maybe RestyleMachine)
fetchRestyleMachine =
    headMaybe . map entityVal <$> rawSql [st|
        SELECT ??
        FROM restyle_machine
        WHERE enabled = t
        ORDER BY RANDOM ()
        LIMIT 1
    |] []

-- | Run a process on a @'RestyleMachine'@s
runRestyleMachine
    :: (HasLogFunc env, HasProcessContext env)
    => RestyleMachine
    -> FilePath
    -> [String]
    -> (String -> RIO env ())
    -> (String -> RIO env ())
    -> RIO env ExitCode
runRestyleMachine machine cmd args fOut fErr = do
    certPath <- setupCertificatesIfMissing machine
    withModifyEnvVars
            (append
                [ ("DOCKER_HOST", restyleMachineHost machine)
                , ("DOCKER_CERT_PATH", pack certPath)
                , ("DOCKER_TLS_VERIFY", "1")
                ]
            )
        $ followProcess cmd args fOut fErr
    where append envs env = env <> Map.fromList envs

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

{-# LANGUAGE QuasiQuotes #-}

module Backend.RestyleMachine
    ( fetchRestyleMachine
    , runRestyleMachine
    )
where

import Backend.Import

import qualified Data.Text.IO as T
import Database.Persist.Sql (rawSql)
import RIO.Directory
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
        WHERE restyle_machine.enabled = ?
        ORDER BY RANDOM ()
        LIMIT 1
    |] [PersistBool True]

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
    certPath <- restyleMachineCertPath machine
    setupCertificatesIfMissing machine certPath

    withExtraEnvVars
            [ ("DOCKER_HOST", restyleMachineHost machine)
            , ("DOCKER_CERT_PATH", pack certPath)
            , ("DOCKER_TLS_VERIFY", "1")
            ]
        $ followProcess cmd args fOut fErr

setupCertificatesIfMissing :: RestyleMachine -> FilePath -> RIO env ()
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

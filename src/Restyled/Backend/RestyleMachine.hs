{-# LANGUAGE QuasiQuotes #-}

module Restyled.Backend.RestyleMachine
    ( runRestyleMachinesCheck
    , fetchRestyleMachine
    , withRestyleMachineEnv

    -- * Exported for testing
    , CheckRestyleMachinesResult(..)
    , checkRestyleMachines
    , withExtraEnvVars
    )
where

import Restyled.Prelude

import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Database.Persist.Sql (rawSql)
import Restyled.Models
import RIO.Directory
    (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import Text.Shakespeare.Text (st)

runRestyleMachinesCheck
    :: (HasLogFunc env, HasDB env, HasProcessContext env) => RIO env ()
runRestyleMachinesCheck = do
    result <- runDB $ checkRestyleMachines $ \(Entity _ machine) -> do
        (ec, _out, _err) <- withRestyleMachineEnv machine
            $ proc "timeout" ["3s", "docker", "info"] readProcess
        pure $ ec == ExitSuccess

    case result of
        AllHealthy -> logInfo "All enabled Restyle Machines are healthy"
        NoneHealthy -> logError "No enabled Restyle Machines are healthy"
        Disabled machines ->
            logWarn
                $ "Disabled "
                <> displayShow (length machines)
                <> " unhealthy machine(s)"

data CheckRestyleMachinesResult
    = AllHealthy
    | NoneHealthy
    | Disabled [Entity RestyleMachine]
    deriving (Eq, Show)

checkRestyleMachines
    :: MonadIO m
    => (Entity RestyleMachine -> m Bool)
    -> SqlPersistT m CheckRestyleMachinesResult
checkRestyleMachines isHealthy = do
    enabled <- selectList [RestyleMachineEnabled ==. True] []
    results <- partitionM (lift . isHealthy) enabled
    case results of
        ([], _) -> pure NoneHealthy
        (_, []) -> pure AllHealthy
        (_, unhealthy) -> do
            updateWhere
                [RestyleMachineId <-. map entityKey unhealthy]
                [RestyleMachineEnabled =. False]
            pure $ Disabled unhealthy

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

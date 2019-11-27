{-# LANGUAGE DerivingVia #-}

module Restyled.Backend.RestyleMachine
    ( runRestyleMachinesCheck
    , withRestyleMachine
    , withRestyleMachineEnv
    , deleteRestyleMachine

    -- * Exported for testing
    , CheckRestyleMachinesResult(..)
    , checkRestyleMachines
    , withExtraEnvVars
    )
where

import Restyled.Prelude

import qualified Data.Map as Map
import Data.Semigroup.Generic
import qualified Data.Text.IO as T
import Restyled.Models
import RIO.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , getHomeDirectory
    , removeDirectoryRecursive
    )
import System.FilePath ((</>))

runRestyleMachinesCheck
    :: (HasLogFunc env, HasDB env, HasProcessContext env) => RIO env ()
runRestyleMachinesCheck = do
    result <- runDB $ checkRestyleMachines $ \(Entity _ machine) -> do
        (ec, _out, _err) <- withRestyleMachineEnv machine
            $ proc "timeout" ["3s", "docker", "info"] readProcess
        pure $ ec == ExitSuccess

    case result of
        AllHealthy -> logInfo "All Restyle Machines healthy"
        NoneHealthy -> logError "No Restyle Machines are healthy"
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
    machines <- selectList [] []
    fromMachineState =<< lift (getRestyleMachinesState isHealthy machines)
  where
    fromMachineState RestyleMachinesState {..}
        | null rmsSteady && null rmsDisabledHealthy = pure NoneHealthy
        | otherwise = do
            updateWhere
                [RestyleMachineId <-. map entityKey rmsDisabledHealthy]
                [RestyleMachineEnabled =. True]
            updateWhere
                [RestyleMachineId <-. map entityKey rmsEnabledUnhealthy]
                [RestyleMachineEnabled =. False]
            pure $ if null rmsEnabledUnhealthy
                then AllHealthy
                else Disabled rmsEnabledUnhealthy

data RestyleMachinesState = RestyleMachinesState
    { rmsEnabledUnhealthy :: [Entity RestyleMachine]
    , rmsDisabledHealthy :: [Entity RestyleMachine]
    , rmsSteady :: [Entity RestyleMachine]
    }
    deriving stock Generic
    deriving (Semigroup, Monoid) via (GenericSemigroupMonoid RestyleMachinesState)

enabledUnhealthyState :: Entity RestyleMachine -> RestyleMachinesState
enabledUnhealthyState machine = mempty { rmsEnabledUnhealthy = [machine] }

disabledHealthyState :: Entity RestyleMachine -> RestyleMachinesState
disabledHealthyState machine = mempty { rmsDisabledHealthy = [machine] }

steadyState :: Entity RestyleMachine -> RestyleMachinesState
steadyState machine = mempty { rmsSteady = [machine] }

getRestyleMachinesState
    :: MonadIO m
    => (Entity RestyleMachine -> m Bool)
    -> [Entity RestyleMachine]
    -> m RestyleMachinesState
getRestyleMachinesState isHealthy = foldMapM go
  where
    go machine@(Entity _ RestyleMachine {..}) = do
        healthy <- isHealthy machine

        pure $ case (restyleMachineEnabled, healthy) of
            (True, True) -> steadyState machine
            (True, False) -> enabledUnhealthyState machine
            (False, True) -> disabledHealthyState machine
            (False, False) -> mempty

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

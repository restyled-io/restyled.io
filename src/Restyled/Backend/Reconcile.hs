module Restyled.Backend.Reconcile
    ( safelyReconcile

    -- * Exported for testing
    , reconcileMachine
    )
where

import Restyled.Prelude

import Restyled.Backend.DockerRunJob (followJobContainer)
import Restyled.Backend.RestyleMachine (withRestyleMachineEnv)
import Restyled.Backend.StoppedContainer
import Restyled.Models

data ReconcileResult = ReconcileResult
    { rrMachineName :: Text
    , rrWarnings :: [String]
    , rrReconciled :: Int
    }

instance Display ReconcileResult where
    display ReconcileResult {..} =
        displayShow rrReconciled
            <> " containers reconciled on "
            <> display rrMachineName
            <> (if null rrWarnings
                   then ""
                   else ", with warning(s): " <> displayShow rrWarnings
               )

safelyReconcile
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => Int
    -- ^ Timeout in seconds
    -> Maybe [Entity RestyleMachine]
    -- ^ Machines to reconcile, 'Nothing' means /all/
    -> m ()
safelyReconcile t mMachines = do
    machines <- maybe (runDB $ selectList [] []) pure mMachines
    meResult <- timeout (t * 1000000) $ tryAny $ runReconcile machines

    case meResult of
        Nothing ->
            logError $ "Timed out after " <> displayShow t <> " second(s)"
        Just (Left ex) -> logError $ displayShow ex
        Just (Right results) -> traverse_ (logInfo . display) results

runReconcile
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => [Entity RestyleMachine]
    -> m [ReconcileResult]
runReconcile = traverse $ \(Entity machineId machine) -> do
    (warnings, reconciled) <- withRestyleMachineEnv machine reconcileMachine

    unless (reconciled == 0) $ runDB $ update
        machineId
        [RestyleMachineJobCount -=. reconciled]

    pure ReconcileResult
        { rrMachineName = restyleMachineName machine
        , rrWarnings = warnings
        , rrReconciled = reconciled
        }

reconcileMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => m ([String], Int)
reconcileMachine =
    either unreconciled reconcileStoppedContainers =<< getStoppedContainers
  where
    unreconciled :: Applicative m => String -> m ([String], Int)
    unreconciled err = pure ([err], 0)

reconcileStoppedContainers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => [StoppedContainer]
    -> m ([String], Int)
reconcileStoppedContainers =
    pure . second sum . partitionEithers <=< traverse reconcileStoppedContainer

reconcileStoppedContainer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => StoppedContainer
    -> m (Either String Int)
reconcileStoppedContainer stoppedContainer@StoppedContainer {..} = do
    mJob <- runDB
        $ selectFirst [JobId ==. scJobId, JobCompletedAt ==. Nothing] []

    case mJob of
        Nothing -> pure $ Left noSuchJob
        Just job -> Right 1 <$ reconcileJob job stoppedContainer
  where
    noSuchJob =
        "No incomplete Job found with Id " <> unpack (toPathPiece scJobId)

reconcileJob
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => Entity Job
    -> StoppedContainer
    -> m ()
reconcileJob job StoppedContainer {..} = do
    mTimestamp <- runDB $ fetchLastJobLogLineCreatedAt scJobId
    void $ followJobContainer mTimestamp job scContainerId
    runDB $ update
        scJobId
        [ JobUpdatedAt =. scFinishedAt
        , JobCompletedAt =. Just scFinishedAt
        , JobExitCode =. Just scExitCode
        ]

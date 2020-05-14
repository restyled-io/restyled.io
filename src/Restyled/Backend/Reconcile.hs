

module Restyled.Backend.Reconcile
    ( runReconcile
    , reconcileMachine
    )
where

import Restyled.Prelude

import Restyled.Backend.Container
import Restyled.Backend.DockerRunJob (followJobContainer)
import Restyled.Backend.RestyleMachine (withRestyleMachineEnv)
import Restyled.Models

runReconcile
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasDB env
       , HasProcessContext env
       )
    => m ()
runReconcile = do
    machines <- runDB $ selectList [] []

    for_ machines $ \(Entity machineId machine) -> do
        (warnings, reconciled) <- withRestyleMachineEnv machine reconcileMachine

        unless (null warnings)
            $ logWarn
            $ "Reconcilation warnings: "
            <> displayShow warnings

        unless (reconciled == 0) $ do
            logInfo $ displayShow reconciled <> " Job(s) reconciled"
            runDB $ update machineId [RestyleMachineJobCount -=. reconciled]

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

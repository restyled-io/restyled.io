module Restyled.Backend.Reconcile
    ( runReconcile
    )
where

import Restyled.Prelude

import Restyled.Backend.DockerRunJob (followJobContainer)
import Restyled.Backend.RestyleMachine (withRestyleMachineEnv)
import Restyled.Backend.StoppedContainer
import Restyled.Models

runReconcile :: (HasLogFunc env, HasDB env, HasProcessContext env) => RIO env ()
runReconcile = do
    openJobs <- runDB
        $ selectList [JobCompletedAt ==. Nothing] [Asc JobCreatedAt]
    traverse_ reconcileJob openJobs

reconcileJob
    :: (HasLogFunc env, HasDB env, HasProcessContext env)
    => Entity Job
    -> RIO env ()
reconcileJob job@(Entity jobId Job {..}) = void $ runMaybeT $ do
    machineName <- hoistMaybe jobMachineName
    containerId <- unpack <$> hoistMaybe jobContainerId
    machine <- MaybeT $ runDB $ getBy $ UniqueRestyleMachine machineName

    withRestyleMachineEnv (entityVal machine) $ do
        stoppedContainer <- getStoppedContainerT containerId

        lift $ do
            mTimestamp <- runDB $ fetchLastJobLogLineCreatedAt jobId
            void $ followJobContainer mTimestamp job containerId
            runDB $ finishJob job machine stoppedContainer

finishJob
    :: MonadIO m
    => Entity Job
    -> Entity RestyleMachine
    -> StoppedContainer
    -> SqlPersistT m ()
finishJob (Entity jobId _) (Entity machineId _) StoppedContainer {..} = do
    update
        jobId
        [ JobUpdatedAt =. scFinishedAt
        , JobCompletedAt =. Just scFinishedAt
        , JobExitCode =. Just scExitCode
        ]
    update machineId [RestyleMachineJobCount -=. 1]

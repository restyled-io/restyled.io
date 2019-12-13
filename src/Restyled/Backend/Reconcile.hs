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
    openJobs <- runDB $ selectList
        [ JobMachineName !=. Nothing
        , JobContainerId !=. Nothing
        , JobCompletedAt ==. Nothing
        ]
        [Asc JobCreatedAt]
    traverse_ reconcileJob openJobs

reconcileJob
    :: (HasLogFunc env, HasDB env, HasProcessContext env)
    => Entity Job
    -> RIO env ()
reconcileJob job@(Entity jobId Job {..}) = void $ runMaybeT $ do
    machineName <- hoistMaybe jobMachineName
    containerId <- unpack <$> hoistMaybe jobContainerId
    machine <- MaybeT $ runDB $ getBy $ UniqueRestyleMachine machineName

    logInfo
        $ fromString
        $ unpack
        $ repoPullPath jobOwner jobRepo jobPullRequest
        <> " in progress on "
        <> machineName
        <> ":"
        <> pack containerId

    withRestyleMachineEnv (entityVal machine) $ do
        stoppedContainer <- getStoppedContainerT containerId

        lift $ do
            mTimestamp <- runDB $ fetchLastJobLogLineCreatedAt jobId

            logInfo
                $ "Reconciling stopped container from "
                <> displayShow mTimestamp
                <> " to "
                <> displayShow (scFinishedAt stoppedContainer)
                <> " (exited "
                <> displayShow (scExitCode stoppedContainer)
                <> ")"

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

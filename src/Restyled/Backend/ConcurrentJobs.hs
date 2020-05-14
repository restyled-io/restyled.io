module Restyled.Backend.ConcurrentJobs
    ( checkConcurrentJobs
    )
where

import Restyled.Prelude

import Restyled.Backend.RestyleMachine
import Restyled.Backend.StoppedContainer
import Restyled.Models

-- | Handle concurrent Jobs for the same PR
--
-- If a concurrent Job exists that is newer than the one given, there is no need
-- to process that Job. This function will throw in that case.
--
-- If a concurrent Job exists that is older than the one given, we should cancel
-- it. Both to free resources and to prevent that Job erroring if it attempts to
-- push a stale resyling up after the in-progress Job finishes.
--
-- Cancelling is accomplished by sending a SIGHUP to the container. It is
-- expected to exit on its own and whatever Backend process is tracking it will
-- do the required cleanup from there.
--
checkConcurrentJobs
    :: (HasLogFunc env, HasProcessContext env, HasDB env)
    => Entity Job
    -- ^ Job about to run
    -> (Entity Job -> e)
    -- ^ Build the error to throw, if stale, given newer Job
    -> ExceptT e (RIO env) ()
checkConcurrentJobs job errStale = do
    mInProgressJob <- lift $ runDB $ fetchJobConcurrentWith job

    for_ mInProgressJob $ \inProgress -> if inProgress `isNewerThan` job
        then throwError $ errStale inProgress
        else lift $ do
            logInfo
                $ "Cancelling Job: "
                <> displayJob job
                <> ", newer job in progress: "
                <> displayJob inProgress
            cancelJob $ entityKey job

isNewerThan :: Entity Job -> Entity Job -> Bool
isNewerThan = (>) `on` (jobCreatedAt . entityVal)

cancelJob
    :: (HasLogFunc env, HasProcessContext env, HasDB env) => JobId -> RIO env ()
cancelJob jobId = do
    machines <- runDB fetchActiveRestyleMachines
    traverse_ (sighupJobOnMachine jobId . entityVal) machines

-- | Send SIGHUP to the Job's container, if found on a given Machine
--
-- TODO: this signal will be ignored for now, but the logging will tell us how
-- often we may use it if we built it.
--
sighupJobOnMachine
    :: (HasLogFunc env, HasProcessContext env)
    => JobId
    -> RestyleMachine
    -> RIO env ()
sighupJobOnMachine jobId machine = do
    logDebug $ "Checking Restyle Machine " <> display name
    withRestyleMachineEnv machine $ do
        emContainer <- getRunningContainer jobId
        either warn (traverse_ $ signalContainerLogged "SIGHUP") emContainer
  where
    name = restyleMachineName machine
    warn err =
        logWarn
            $ "Error getting running container on "
            <> display name
            <> " "
            <> fromString err

displayJob :: Entity Job -> Utf8Builder
displayJob jobE@(Entity jobId job) =
    display (jobPath jobE)
        <> " id="
        <> display (toPathPiece jobId)
        <> " createdAt="
        <> displayShow (jobCreatedAt job)

fetchJobConcurrentWith
    :: MonadIO m => Entity Job -> SqlPersistT m (Maybe (Entity Job))
fetchJobConcurrentWith (Entity jobId job) = selectFirst
    [ JobOwner ==. jobOwner job
    , JobRepo ==. jobRepo job
    , JobPullRequest ==. jobPullRequest job
    , JobCompletedAt ==. Nothing
    , JobId !=. jobId
    ]
    [Asc JobCreatedAt]

fetchActiveRestyleMachines :: MonadIO m => SqlPersistT m [Entity RestyleMachine]
fetchActiveRestyleMachines =
    selectList [RestyleMachineEnabled ==. True, RestyleMachineJobCount >. 0] []

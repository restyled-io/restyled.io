module Restyled.Backend.ConcurrentJobs
    ( StaleJobs
    , checkConcurrentJobs
    , cancelStaleJobs
    )
where

import Restyled.Prelude

import Restyled.Backend.RestyleMachine
import Restyled.Backend.StoppedContainer
import Restyled.Models

newtype StaleJobs = StaleJobs
    { unStaleJobs :: [Entity Job]
    }

-- | Handle concurrent Jobs for the same PR
--
-- If a concurrent Job exists that is newer than the one given, there is no need
-- to process that Job. This function will throw in that case.
--
-- If concurrent Jobs exist that are older than the one given, we should cancel
-- those. Both to free resources and to prevent that Job erroring if it attempts to
-- push a stale resytling up after the in-progress Job finishes.
--
checkConcurrentJobs
    :: (MonadUnliftIO m, MonadReader env m, HasDB env)
    => Entity Job
    -- ^ Job about to run
    -> (Entity Job -> e)
    -- ^ Build the error to throw, if stale, given oldest newer Job
    -> ExceptT e m StaleJobs
    -- ^ Returns any older Jobs, for potential cancellation
checkConcurrentJobs job errStale = do
    (newer, older) <-
        lift
        $ runDB
        $ partition (`isNewerThan` job)
        <$> fetchJobsConcurrentWith job

    case newer of
        [] -> pure $ StaleJobs older
        (x : _) -> throwError $ errStale x

isNewerThan :: Entity Job -> Entity Job -> Bool
isNewerThan = (>) `on` (jobCreatedAt . entityVal)

-- | Cancel 'StaleJob's
--
-- Attempts to find a Container running the Job (known via @label@s) and sends a
-- SIGHUP signal to it. It is expected to exit on its own and whatever Backend
-- process is tracking it will do the required cleanup from there.
--
-- TODO: this signal will be ignored for now, but the logging will tell us how
-- often we may use it if we built it. We could change nothing and just send
-- TERM (or KILL). This would materialize as an error (and potentially red PR),
-- but the newer Job would replace that status. This would be more attractive if
-- we send Pending... :thinking:
--
cancelStaleJobs
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasDB env
       )
    => StaleJobs
    -> m ()
cancelStaleJobs = go . unStaleJobs
  where
    go [] = logDebug "No Stale Jobs to cancel"
    go jobs = do
        machines <- runDB fetchActiveRestyleMachines
        logDebug
            $ "Checking "
            <> displayShow (length machines)
            <> " active Restyle Machine(s)"

        for_ (map entityKey jobs) $ \jobId -> do
            logInfo $ "Cancelling Job #" <> display (toPathPiece jobId)
            traverse_ (sighupJobOnMachine jobId . entityVal) machines

sighupJobOnMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => JobId
    -> RestyleMachine
    -> m ()
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

fetchJobsConcurrentWith :: MonadIO m => Entity Job -> SqlPersistT m [Entity Job]
fetchJobsConcurrentWith (Entity jobId job) = selectList
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

module Restyled.Backend.ConcurrentJobs
    ( StaleJobs
    , checkConcurrentJobs
    , cancelStaleJobs
    )
where

import Restyled.Prelude

import Restyled.Backend.Container
import Restyled.Backend.RestyleMachine
import Restyled.Models
import Restyled.TimeRange

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
    => TimeRange
    -- ^ Time range to check within
    -> Entity Job
    -- ^ Job about to run
    -> (Entity Job -> e)
    -- ^ Build the error to throw if stale, given oldest newer Job
    -> ExceptT e m StaleJobs
    -- ^ Returns any older Jobs, for potential cancellation
checkConcurrentJobs range job errStale = do
    (newer, older) <-
        lift
        $ runDB
        $ partition (`isNewerThan` job)
        <$> fetchJobsConcurrentWith range job

    case newer of
        [] -> pure $ StaleJobs older
        (x : _) -> throwError $ errStale x

isNewerThan :: Entity Job -> Entity Job -> Bool
isNewerThan = (>) `on` (jobCreatedAt . entityVal)

-- | Cancel 'StaleJob's
--
-- Attempts to find a Container running the Job (known via @label@s) and sends a
-- SIGQUIT signal to it. It is expected to exit on its own and whatever Backend
-- process is tracking it will do the required cleanup from there.
--
-- TODO: this signal is ignored at the moment, but the logged attempt can tell
-- us how often it would kick in if we built it.
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
    go
        :: ( MonadUnliftIO m
           , MonadReader env m
           , HasLogFunc env
           , HasProcessContext env
           , HasDB env
           )
        => [Entity Job]
        -> m ()
    go [] = logDebug "No Stale Jobs to cancel"
    go jobs = do
        machines <- runDB fetchActiveRestyleMachines
        logDebug
            $ "Checking "
            <> displayShow (length machines)
            <> " active Restyle Machine(s)"

        for_ jobs $ \job@(Entity jobId _) -> do
            logInfo $ "Cancelling " <> display job
            traverse_ (cancelJobOnMachine jobId . entityVal) machines

cancelJobOnMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => JobId
    -> RestyleMachine
    -> m ()
cancelJobOnMachine jobId machine = do
    logDebug $ "Checking Restyle Machine " <> display name
    withRestyleMachineEnv machine $ do
        emContainer <- getRunningContainer jobId
        either warn (traverse_ $ signalContainer "SIGQUIT") emContainer
  where
    name = restyleMachineName machine
    warn err =
        logWarn
            $ "Error getting running container on "
            <> display name
            <> " "
            <> fromString err

fetchJobsConcurrentWith
    :: MonadIO m => TimeRange -> Entity Job -> SqlPersistT m [Entity Job]
fetchJobsConcurrentWith range (Entity jobId job) = selectList
    ([ JobOwner ==. jobOwner job
     , JobRepo ==. jobRepo job
     , JobPullRequest ==. jobPullRequest job
     , JobCompletedAt ==. Nothing
     , JobId !=. jobId
     ]
    <> timeRangeFilters JobCreatedAt range
    )
    [Asc JobCreatedAt]

fetchActiveRestyleMachines :: MonadIO m => SqlPersistT m [Entity RestyleMachine]
fetchActiveRestyleMachines =
    selectList [RestyleMachineEnabled ==. True, RestyleMachineJobCount >. 0] []

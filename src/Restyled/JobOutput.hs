module Restyled.JobOutput
    ( fetchJobOutput
    , followJobOutput
    ) where

import Restyled.Prelude

import Conduit
import Data.Semigroup (Last(..))
import qualified Restyled.JobLogLine as CW
import Restyled.Models.DB
import Restyled.Models.Job (jobIsCloudWatch)
import qualified Restyled.Models.JobLogLine as JL
import Restyled.Settings

-- | Return all of the Job's log that we have now and stop
fetchJobOutput
    :: ( MonadUnliftIO m
       , MonadAWS m
       , MonadReader env m
       , HasSettings env
       , HasSqlPool env
       )
    => Entity Job
    -> m [JobLogLine]
fetchJobOutput job =
    runConduit $ streamJobLogLines job Nothing .| concatC .| sinkList

-- | Stream a Job's log until it is finished
--
-- Streams an initial batch of all logs we have right now, then starts streaming
-- batchs of one line at a time as long as the Job is in-progress.
--
followJobOutput
    :: ( MonadUnliftIO m
       , MonadAWS m
       , MonadReader env m
       , HasSqlPool env
       , HasSettings env
       )
    => Entity Job
    -> ConduitT [JobLogLine] Void m (Maybe (Last UTCTime))
    -- ^ A sink to feed the batches of log-lines into
    --
    -- It must return the last created-at, so we can loop from there.
    --
    -> m ()
followJobOutput job@(Entity jobId _) sink = runConduit $ loop Nothing
  where
    loop mSince = do
        mLastCreatedAt <- streamJobLogLines job (getLast <$> mSince) .| sink

        inProgress <-
            lift
            $ runDB
            $ maybe False (isNothing . jobCompletedAt)
            <$> get jobId

        let continue = inProgress || isJust mLastCreatedAt
            mNextSince = mSince <> mLastCreatedAt

        when continue $ loop mNextSince

-- | Paper over CloudWatch vs DB, for now
streamJobLogLines
    :: ( MonadUnliftIO m
       , MonadAWS m
       , MonadReader env m
       , HasSettings env
       , HasSqlPool env
       )
    => Entity Job
    -> Maybe UTCTime
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines (Entity jobId job) mSince
    | jobIsCloudWatch job = CW.streamJobLogLines jobId mSince
    | otherwise = JL.streamJobLogLines jobId mSince

module Restyled.JobOutput
    ( fetchJobOutput
    , followJobOutput
    ) where

import Restyled.Prelude

import Conduit
import qualified Data.List.NonEmpty as NE
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
    runConduit $ streamJobLogLines job Nothing Nothing .| concatC .| sinkList

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
    -> ConduitT [JobLogLine] [JobLogLine] m ()
    -- ^ A Conduit to feed the batches of log-lines through
    --
    -- For example, to fire off to a WebSocket. We need to get them back again
    -- so we can track when we reach the of the log.
    --
    -> m ()
followJobOutput job@(Entity jobId _) conduit = runConduit
    $ loop Nothing Nothing
  where
    loop mSince mPageSize = do
        mLastCreatedAt <-
            streamJobLogLines job (getLast <$> mSince) mPageSize
            .| conduit
            .| sinkLastBy jobLogLineCreatedAt

        inProgress <-
            lift
            $ runDB
            $ maybe False (isNothing . jobCompletedAt)
            <$> get jobId

        let continue = inProgress || isJust mLastCreatedAt
            mNextSince = mSince <> mLastCreatedAt

        -- Stream 1-line pages from now one
        when continue $ loop mNextSince $ Just 1

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
    -> Maybe Natural
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines (Entity jobId job) mSince mPageSize
    | jobIsCloudWatch job = CW.streamJobLogLines jobId mSince mPageSize
    | otherwise = JL.streamJobLogLines jobId mSince mPageSize

sinkLastBy :: Monad m => (a -> b) -> ConduitT [a] Void m (Maybe (Last b))
sinkLastBy f = foldMapC $ fmap (Last . f . NE.last) . NE.nonEmpty

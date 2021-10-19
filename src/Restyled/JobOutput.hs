module Restyled.JobOutput
    ( fetchJobOutput
    , followJobOutput
    ) where

import Restyled.Prelude

import Conduit
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last(..))
import Restyled.JobLogLine
import Restyled.Models.DB
import Restyled.Settings

-- | Return all of the Job's log that we have now and stop
fetchJobOutput
    :: (MonadUnliftIO m, MonadAWS m, MonadReader env m, HasSettings env)
    => JobId
    -> m [JobLogLine]
fetchJobOutput jobId =
    runConduit $ streamJobLogLines jobId Nothing .| concatC .| sinkList

-- | Stream a Job's log until it is finished
followJobOutput
    :: ( MonadUnliftIO m
       , MonadAWS m
       , MonadReader env m
       , HasSqlPool env
       , HasSettings env
       )
    => JobId
    -> ([JobLogLine] -> m ())
    -- ^ Action to take with each batch of log-lines
    -> m ()
followJobOutput jobId f = loop Nothing
  where
    loop mSince = do
        mLastCreatedAt <-
            runConduit
            $ streamJobLogLines jobId (getLast <$> mSince)
            .| iterMC f
            .| foldMapC getLastCreatedAt

        inProgress <-
            runDB $ maybe False (isNothing . jobCompletedAt) <$> get jobId

        let continue = inProgress || isJust mLastCreatedAt
            mNextSince = mSince <> mLastCreatedAt

        when continue $ loop mNextSince

getLastCreatedAt :: [JobLogLine] -> Maybe (Last UTCTime)
getLastCreatedAt = fmap (Last . jobLogLineCreatedAt . NE.last) . NE.nonEmpty

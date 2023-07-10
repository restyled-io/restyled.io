{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobOutput
  ( fetchJobOutput
  , followJobOutput
  ) where

import Restyled.Prelude hiding (Last (..))

import Amazonka.CloudWatchLogs.GetLogEvents
import Amazonka.CloudWatchLogs.Types
import Amazonka.Pager (AWSPager (..))
import Conduit
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Lens.Micro ((.~), (?~), (^.))
import Restyled.AWS (HasAWS)
import qualified Restyled.AWS as AWS
import Restyled.DB
import Restyled.Models
import Restyled.Settings
import Restyled.Time

-- | Return all of the Job's log that we have now and stop
fetchJobOutput
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasSettings env
     , HasAWS env
     )
  => Entity Job
  -> m [JobLogLine]
fetchJobOutput (Entity jobId Job {..}) = do
  expired <- (jobUpdatedAt <=) <$> getLogRetentionStart
  if expired
    then pure []
    else runConduit $ streamJobLogLines jobId Nothing .| concatC .| sinkList

getLogRetentionStart :: MonadIO m => m UTCTime
getLogRetentionStart = subtractTime (Days $ 30 + 5) <$> getCurrentTime

-- | Stream a Job's log so long as it is in progress
followJobOutput
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasSettings env
     , HasSqlPool env
     , HasAWS env
     )
  => Entity Job
  -> ([JobLogLine] -> m ())
  -- ^ Action to take with each batch of log-lines
  -> m ()
followJobOutput (Entity jobId job) f = do
  expired <- (jobUpdatedAt job <=) <$> getLogRetentionStart
  if expired then pure () else loop Nothing
 where
  loop mSince = do
    mLastCreatedAt <-
      runConduit
        $ streamJobLogLines jobId (getLast <$> mSince)
        .| iterMC f
        .| foldMapC getLastCreatedAt

    inProgress <-
      runDB $ maybe False (isNothing . jobCompletedAt) <$> get jobId

    let
      continue = inProgress || isJust mLastCreatedAt
      mNextSince = mSince <> mLastCreatedAt

    when continue $ loop mNextSince

getLastCreatedAt :: [JobLogLine] -> Maybe (Last UTCTime)
getLastCreatedAt = fmap (Last . jobLogLineCreatedAt . NE.last) . NE.nonEmpty

-- TODO: upstream?
instance AWSPager GetLogEvents where
  page req resp = do
    -- Events were present in last response
    events <- resp ^. getLogEventsResponse_events
    guard $ not $ null events

    -- Forward token present, and differs from what we just used
    nextToken <- resp ^. getLogEventsResponse_nextForwardToken
    guard $ req ^. getLogEvents_nextToken /= Just nextToken

    pure $ req & (getLogEvents_nextToken ?~ nextToken)

streamJobLogLines
  :: (MonadResource m, MonadReader env m, HasAWS env, HasSettings env)
  => JobId
  -> Maybe UTCTime
  -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince = do
  AppSettings {..} <- lift $ view settingsL

  let
    groupName = appRestylerLogGroup
    streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
    req =
      newGetLogEvents groupName streamName
        & (getLogEvents_startTime .~ startMilliseconds)
        & (getLogEvents_startFromHead ?~ True)

  AWS.paginate req .| mapC fromGetLogEvents
 where
  startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

fromGetLogEvents :: GetLogEventsResponse -> [JobLogLine]
fromGetLogEvents resp =
  mapMaybe fromOutputLogEvent
    $ fromMaybe []
    $ resp
    ^. getLogEventsResponse_events

fromOutputLogEvent :: OutputLogEvent -> Maybe JobLogLine
fromOutputLogEvent event = do
  message <- event ^. outputLogEvent_message
  timestamp <- event ^. outputLogEvent_timestamp
  pure $ jobLogLine (posixMillisecondsToUTCTime timestamp) message

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

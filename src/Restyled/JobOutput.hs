{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobOutput
    ( fetchJobOutput
    , followJobOutput
    ) where

import Restyled.Prelude

import Conduit
import Control.Lens ((?~))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last(..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Pager (AWSPager(..))
import Restyled.Models
import Restyled.Settings

-- | Return all of the Job's log that we have now and stop
fetchJobOutput
    :: (MonadUnliftIO m, MonadAWS m, MonadReader env m, HasSettings env)
    => JobId
    -> m [JobLogLine]
fetchJobOutput jobId =
    runConduit $ streamJobLogLines jobId Nothing .| concatC .| sinkList

-- | Stream a Job's log so long as it is in progress
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

-- TODO: upstream?
instance AWSPager GetLogEvents where
    page req resp = do
        -- Events were present in last response
        guard $ not $ null $ resp ^. glersEvents

        -- Forward token present, and differs from what we just used
        nextToken <- resp ^. glersNextForwardToken
        guard $ req ^. gleNextToken /= Just nextToken

        pure $ req & (gleNextToken ?~ nextToken)

streamJobLogLines
    :: (MonadAWS m, MonadReader env m, HasSettings env)
    => JobId
    -> Maybe UTCTime
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince = do
    AppSettings {..} <- lift $ view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
        req =
            getLogEvents groupName streamName
                & (gleStartTime .~ startMilliseconds)
                & (gleStartFromHead ?~ True)

    paginate req .| mapC fromGetLogEvents
    where startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

fromGetLogEvents :: GetLogEventsResponse -> [JobLogLine]
fromGetLogEvents resp = mapMaybe fromOutputLogEvent $ resp ^. glersEvents

fromOutputLogEvent :: OutputLogEvent -> Maybe JobLogLine
fromOutputLogEvent event = do
    message <- event ^. oleMessage
    timestamp <- event ^. oleTimestamp
    pure $ jobLogLine (posixMillisecondsToUTCTime timestamp) message

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

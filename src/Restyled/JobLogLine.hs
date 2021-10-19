{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobLogLine
    ( JobLogLine(..)
    , streamJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Control.Lens ((?~))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Pager (AWSPager(..))
import Restyled.Models.DB (JobId)
import Restyled.Settings

instance AWSPager GetLogEvents where
    page req resp = do
        -- Events were present in last response
        guard $ not $ null $ resp ^. glersEvents

        -- Forward token present, and differs from what we just used
        nextToken <- resp ^. glersNextForwardToken
        guard $ req ^. gleNextToken /= Just nextToken

        pure $ req & (gleNextToken ?~ nextToken)

data JobLogLine = JobLogLine
    { jobLogLineCreatedAt :: UTCTime
    , jobLogLineContent :: Text
    }

-- | Paginate the Job log from CloudWatch in a streaming fashion
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
    pure JobLogLine
        { jobLogLineCreatedAt = posixMillisecondsToUTCTime timestamp
        , jobLogLineContent = message
        }

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

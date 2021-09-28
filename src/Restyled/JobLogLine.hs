{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobLogLine
    ( streamJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Control.Lens ((?~))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Pager (AWSPager(..))
import Restyled.Models.DB (JobId, JobLogLine(..))
import Restyled.Settings

instance AWSPager GetLogEvents where
    page req resp = do
        -- Events were present in last response
        guard $ not $ null $ resp ^. glersEvents

        -- Forward token present, and differs from what we just used
        nextToken <- resp ^. glersNextForwardToken
        guard $ req ^. gleNextToken /= Just nextToken

        pure $ req & (gleNextToken ?~ nextToken)

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

    paginate req .| mapC (fromGetLogEvents jobId)
    where startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

fromGetLogEvents :: JobId -> GetLogEventsResponse -> [JobLogLine]
fromGetLogEvents jobId resp =
    mapMaybe (fromOutputLogEvent jobId) $ resp ^. glersEvents

fromOutputLogEvent :: JobId -> OutputLogEvent -> Maybe JobLogLine
fromOutputLogEvent jobId event = do
    message <- event ^. oleMessage
    timestamp <- event ^. oleTimestamp
    pure JobLogLine
        { jobLogLineCreatedAt = posixMillisecondsToUTCTime timestamp
        , jobLogLineStream = "system"
        , jobLogLineContent = message
        , jobLogLineJob = jobId
        }

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

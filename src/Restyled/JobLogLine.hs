{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobLogLine
    ( JobLogLine(..)
    , fetchJobLogLines
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

data JobLogStream
    = JobLogStreamSystem
    | JobLogStreamStdout
    | JobLogStreamStderr

jobLogStreamToText :: JobLogStream -> Text
jobLogStreamToText = \case
    JobLogStreamSystem -> "system"
    JobLogStreamStdout -> "stdout"
    JobLogStreamStderr -> "stderr"

instance AWSPager GetLogEvents where
    page req resp = do
        -- Events were present in last response
        guard $ not $ null $ resp ^. glersEvents

        -- Forward token present, and differs from what we just used
        nextToken <- resp ^. glersNextForwardToken
        guard $ req ^. gleNextToken /= Just nextToken

        pure $ req & (gleNextToken ?~ nextToken)

fetchJobLogLines
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSettings env
       , HasAWS env
       )
    => JobId
    -> Maybe UTCTime
    -> m [JobLogLine]
fetchJobLogLines jobId mSince = handleAny (errorJobLogLines jobId) $ do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
        req =
            getLogEvents groupName streamName
                & (gleStartTime .~ startMilliseconds)
                & (gleStartFromHead ?~ True)
    pageAWS req $ concatMapC (fromGetLogEvents jobId) .| sinkList
    where startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

errorJobLogLines
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Show ex)
    => JobId
    -> ex
    -> m [JobLogLine]
errorJobLogLines jobId ex = do
    logError
        $ "Error fetching Job log for Job "
        <> display (toPathPiece jobId)
        <> ": "
        <> displayShow ex
    now <- liftIO getCurrentTime
    pure
        [ JobLogLine
              { jobLogLineCreatedAt = now
              , jobLogLineStream = jobLogStreamToText JobLogStreamSystem
              , jobLogLineContent = "Unable to fetch Job log at this time"
              , jobLogLineJob = jobId
              }
        ]

fromGetLogEvents :: JobId -> GetLogEventsResponse -> [JobLogLine]
fromGetLogEvents jobId resp =
    mapMaybe (fromOutputLogEvent jobId) $ resp ^. glersEvents

fromOutputLogEvent :: JobId -> OutputLogEvent -> Maybe JobLogLine
fromOutputLogEvent jobId event = do
    message <- event ^. oleMessage
    timestamp <- event ^. oleTimestamp
    pure JobLogLine
        { jobLogLineCreatedAt = posixMillisecondsToUTCTime timestamp
        , jobLogLineStream = jobLogStreamToText JobLogStreamSystem
        , jobLogLineContent = message
        , jobLogLineJob = jobId
        }

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

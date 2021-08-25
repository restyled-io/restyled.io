module Restyled.JobLogLine
    ( JobLogLine(..)
    , fetchJobLogLines
    ) where

import Restyled.Prelude

import Control.Lens ((?~))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.Types
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

fetchJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => JobId
    -> Maybe UTCTime
    -> m [JobLogLine]
fetchJobLogLines jobId mSince = handleAny (\_ -> pure []) $ do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId

    go [] groupName streamName Nothing
  where
    -- We consider "since" to be exclusive, but AWS does not. This means that if
    -- you naively use timestamp of the last event as input to find events since
    -- it, you will continually get that event again. We add a ms to avoid this.
    startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

    go acc groupName streamName mNextToken = do
        resp <-
            runAWS
            $ getLogEvents groupName streamName
            & (gleStartTime .~ startMilliseconds)
            & (gleStartFromHead ?~ True)
            & (gleNextToken .~ mNextToken)

        let events = resp ^. glersEvents

        if null events
            then pure acc
            else go
                (acc <> mapMaybe (fromOutputLogEvent jobId) events)
                groupName
                streamName
                (resp ^. glersNextForwardToken)

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

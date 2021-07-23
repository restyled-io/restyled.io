module Restyled.JobLogLine
    ( JobLogLine(..)
    , jobLogLine
    , jobLogLineAsOf
    , JobLogStream(..)
    , jobLogStreamToText
    , fetchJobLogLines
    , fetchJobLogLinesSince
    , captureJobLogLine
    , captureJobLogLines
    ) where

import Restyled.Prelude

import Control.Lens ((?~))
import Data.List (find)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DescribeLogGroups
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.PutLogEvents
import Network.AWS.CloudWatchLogs.Types
import Restyled.Models.DB (JobId, JobLogLine(..))
import Restyled.Settings

jobLogLine :: MonadIO m => JobId -> JobLogStream -> Text -> m JobLogLine
jobLogLine jobId stream content = do
    now <- liftIO getCurrentTime
    pure $ jobLogLineAsOf now jobId stream content

jobLogLineAsOf :: UTCTime -> JobId -> JobLogStream -> Text -> JobLogLine
jobLogLineAsOf now jobId stream content = JobLogLine
    { jobLogLineCreatedAt = now
    , jobLogLineStream = jobLogStreamToText stream
    , jobLogLineContent = content
    , jobLogLineJob = jobId
    }

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
    -> m [JobLogLine]
fetchJobLogLines = fetchJobLogLinesSince Nothing

fetchJobLogLinesSince
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => Maybe UTCTime
    -> JobId
    -> m [JobLogLine]
fetchJobLogLinesSince mStartTime jobId = handleAny (\_ -> pure []) $ do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId

    go [] groupName streamName Nothing
  where
    -- We consider "since" to be exclusive, but AWS does not. This means that if
    -- you naively use timestamp of the last event as input to find events since
    -- it, you will continually get that event again. We add a ms to avoid this.
    startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mStartTime

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

newtype JobLogError
    = JobLogErrorGroupDoesNotExist Text
    deriving stock (Eq, Show)

instance Exception JobLogError where
    displayException = \case
        JobLogErrorGroupDoesNotExist name ->
            "LogGroup does not exist with name: " <> show name

captureJobLogLine
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
    => JobId
    -> JobLogStream
    -> Text
    -> m ()
captureJobLogLine jobId stream content = do
    jobLogLines <- pure <$> jobLogLine jobId stream content
    captureJobLogLines jobId jobLogLines

captureJobLogLines
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
    => JobId
    -> NonEmpty JobLogLine
    -> m ()
captureJobLogLines jobId jobLogLines = do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
        events = toInputLogEvent <$> jobLogLines

    mSequenceToken <- findOrCreateLogStream groupName streamName

    void
        $ runAWS
        $ putLogEvents groupName streamName events
        & (pleSequenceToken .~ mSequenceToken)

toInputLogEvent :: JobLogLine -> InputLogEvent
toInputLogEvent JobLogLine {..} = inputLogEvent
    (utcTimeToPOSIXMilliseconds jobLogLineCreatedAt)
    jobLogLineContent

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

findOrCreateLogStream
    :: (MonadIO m, MonadReader env m, HasAWS env)
    => Text -- ^ Group name
    -> Text -- ^ Stream name
    -> m (Maybe Text) -- ^ Sequence token if existed
findOrCreateLogStream group name = do
    resp <-
        runAWS
        $ describeLogStreams group
        & (dlssLogStreamNamePrefix ?~ name)
        & (dlssLimit ?~ 1)

    let
        mStream = find
            ((== Just name) . (^. lsLogStreamName))
            (resp ^. dlsrsLogStreams)

    case mStream of
        Nothing -> do
            assertLogGroupExists group
            Nothing <$ runAWS (createLogStream group name)
        Just stream -> pure $ stream ^. lsUploadSequenceToken

assertLogGroupExists
    :: (MonadIO m, MonadReader env m, HasAWS env) => Text -> m ()
assertLogGroupExists desiredName = do
    resp <-
        runAWS
        $ describeLogGroups
        & (dlgLogGroupNamePrefix ?~ desiredName)
        & (dlgLimit ?~ 1)

    let
        groupExists = isJust $ find
            ((== Just desiredName) . (^. lgLogGroupName))
            (resp ^. dlgrsLogGroups)

    unless groupExists $ throwIO $ JobLogErrorGroupDoesNotExist desiredName

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

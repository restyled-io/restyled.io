{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobLogLine
    ( streamJobLogLines
    , captureJobLogLines
    , deleteJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Control.Lens ((?~))
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.AWS.CloudWatchLogs.CreateLogStream
import Network.AWS.CloudWatchLogs.DeleteLogStream
import Network.AWS.CloudWatchLogs.DescribeLogStreams
import Network.AWS.CloudWatchLogs.GetLogEvents
import Network.AWS.CloudWatchLogs.PutLogEvents
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
    -> Maybe UTCTime -- ^ Stream logs since a given time
    -> Maybe Natural -- ^ Page size, /not/ overall limit
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince mPageSize = do
    AppSettings {..} <- lift $ view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
        req =
            getLogEvents groupName streamName
                & (gleStartTime .~ startMilliseconds)
                & (gleStartFromHead ?~ True)
                & (gleLimit .~ mPageSize)

    paginate req .| mapC (fromGetLogEvents jobId)
    where startMilliseconds = (+ 1) . utcTimeToPOSIXMilliseconds <$> mSince

captureJobLogLines
    :: (MonadAWS m, MonadReader env m, HasSettings env) => [JobLogLine] -> m ()
captureJobLogLines allJobLogLines = do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup

    for_ (NE.groupAllWith jobLogLineJob allJobLogLines) $ \jobLogLines -> do
        let jobId = jobLogLineJob $ NE.head jobLogLines
            streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
            events = toInputLogEvent <$> jobLogLines

        mSequenceToken <- findOrCreateLogStream groupName streamName

        void
            $ send
            $ putLogEvents groupName streamName events
            & (pleSequenceToken .~ mSequenceToken)

deleteJobLogLines
    :: (MonadAWS m, MonadReader env m, HasSettings env) => JobId -> m ()
deleteJobLogLines jobId = do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId

    void $ send $ deleteLogStream groupName streamName

toInputLogEvent :: JobLogLine -> InputLogEvent
toInputLogEvent JobLogLine {..} = inputLogEvent
    (utcTimeToPOSIXMilliseconds jobLogLineCreatedAt)
    jobLogLineContent

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

findOrCreateLogStream
    :: (MonadIO m, MonadAWS m)
    => Text -- ^ Group name
    -> Text -- ^ Stream name
    -> m (Maybe Text) -- ^ Sequence token if existed
findOrCreateLogStream group name = do
    resp <-
        send
        $ describeLogStreams group
        & (dlssLogStreamNamePrefix ?~ name)
        & (dlssLimit ?~ 1)

    let
        mStream = find
            ((== Just name) . (^. lsLogStreamName))
            (resp ^. dlsrsLogStreams)

    case mStream of
        Nothing -> Nothing <$ send (createLogStream group name)
        Just stream -> pure $ stream ^. lsUploadSequenceToken

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

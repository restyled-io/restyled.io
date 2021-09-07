{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.JobLogLine
    ( JobLogLine(..)
    , fetchJobLogLines
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
              , jobLogLineStream = "system"
              , jobLogLineContent = "Unable to fetch Job log at this time"
              , jobLogLineJob = jobId
              }
        ]

captureJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => [JobLogLine]
    -> m ()
captureJobLogLines allJobLogLines = do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup

    for_ (NE.groupAllWith jobLogLineJob allJobLogLines) $ \jobLogLines -> do
        let jobId = jobLogLineJob $ NE.head jobLogLines
            streamName = appRestylerLogStreamPrefix <> toPathPiece jobId
            events = toInputLogEvent <$> jobLogLines

        mSequenceToken <- findOrCreateLogStream groupName streamName

        void
            $ runAWS
            $ putLogEvents groupName streamName events
            & (pleSequenceToken .~ mSequenceToken)

deleteJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => JobId
    -> m ()
deleteJobLogLines jobId = do
    AppSettings {..} <- view settingsL

    let groupName = appRestylerLogGroup
        streamName = appRestylerLogStreamPrefix <> toPathPiece jobId

    void $ runAWS $ deleteLogStream groupName streamName

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
        Nothing -> Nothing <$ runAWS (createLogStream group name)
        Just stream -> pure $ stream ^. lsUploadSequenceToken

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

posixMillisecondsToUTCTime :: Integral n => n -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

{-# LANGUAGE NoFieldSelectors #-}

module Restyled.Development.LogEvent
  ( putLogEvents
  , LogEventErrors (..)
  , LogEventError (..)
  , validateLogEvents
  , validateLogEvent
  ) where

import Restyled.Prelude hiding (group)

import Amazonka.CloudWatchLogs.PutLogEvents
import Amazonka.CloudWatchLogs.Types.InputLogEvent
import Amazonka.CloudWatchLogs.Types.RejectedLogEventsInfo
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Validate
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro (to, (.~), (^.))
import Restyled.AWS (HasAWS (..))
import qualified Restyled.AWS as AWS

newtype LogEventErrors = LogEventErrors
  { unwrap :: NonEmpty LogEventError
  }
  deriving stock (Show)
  deriving newtype (Semigroup)

instance Exception LogEventErrors where
  displayException = \case
    LogEventErrors (ex :| []) -> displayException ex
    LogEventErrors (ex :| exs) ->
      intercalate "\n" $ map ((" • " <>) . displayException) $ ex : exs

logEventError :: LogEventError -> LogEventErrors
logEventError = LogEventErrors . pure

data LogEventError
  = NoEvents
  | NotChronological
  | SpansTooFar
  | TooManyEvents
  | EventEmpty InputLogEvent
  | EventTooLarge InputLogEvent
  | EventExpired InputLogEvent
  | EventTooNew InputLogEvent
  | EventTooOld InputLogEvent
  deriving stock (Show)

instance Exception LogEventError where
  displayException = \case
    NoEvents -> "No events"
    NotChronological -> "Events no chronological by timestamp"
    SpansTooFar -> "Events span more than " <> show spanHoursLimit <> " hours"
    TooManyEvents -> "Events span more than " <> show spanHoursLimit <> " hours"
    EventEmpty _ -> "Event message empty"
    EventTooLarge _ -> "Event message is more than " <> show messageLimit <> " characters"
    EventExpired _ -> "Event is expired"
    EventTooNew event ->
      "Event timestamp ("
        <> show (event ^. inputLogEvent_timestamp . to msToTime)
        <> ") is more than "
        <> show futureHoursLimit
        <> " hours in the future"
    EventTooOld event ->
      "Event timestamp ("
        <> show (event ^. inputLogEvent_timestamp . to msToTime)
        <> ") is more than "
        <> show pastDaysLimit
        <> " days in the past"

eventsLimit :: Int
eventsLimit = 10000

spanHoursLimit :: Double
spanHoursLimit = 24

futureHoursLimit :: Int
futureHoursLimit = 2

pastDaysLimit :: Int
pastDaysLimit = 14

messageLimit :: Int
messageLimit = 10000

putLogEvents
  :: (MonadResource m, MonadReader env m, HasAWS env)
  => Text
  -> Text
  -> [InputLogEvent]
  -> ExceptT LogEventErrors m ()
putLogEvents group stream = go Nothing
 where
  go mtoken events = do
    if length events > eventsLimit
      then do
        let (batch, leftover) = splitAt eventsLimit events
        resp <- putLogEventsBatch group stream mtoken batch
        let mnext = resp ^. putLogEventsResponse_nextSequenceToken
        go mnext leftover
      else void $ putLogEventsBatch group stream mtoken events

putLogEventsBatch
  :: (MonadResource m, MonadReader env m, HasAWS env)
  => Text
  -> Text
  -> Maybe Text
  -> [InputLogEvent]
  -> ExceptT LogEventErrors m PutLogEventsResponse
putLogEventsBatch group stream mtoken batch = do
  now <- liftIO getCurrentTime
  neBatch <- validateToError $ validateLogEvents now batch
  resp <-
    AWS.send
      $ newPutLogEvents group stream neBatch
      & (putLogEvents_sequenceToken .~ mtoken)

  for_ (resp ^. putLogEventsResponse_rejectedLogEventsInfo)
    $ validateToError
    . refuteRejectedEvents neBatch

  pure resp

validateLogEvents
  :: MonadValidate LogEventErrors m
  => UTCTime
  -> [InputLogEvent]
  -> m (NonEmpty InputLogEvent)
validateLogEvents now events = do
  neEvents <- maybe (refute $ logEventError NoEvents) pure $ NE.nonEmpty events

  -- The log events in the batch must be in chronological order by their timestamp
  when (sortOn (^. inputLogEvent_timestamp) events /= events)
    $ dispute
    $ logEventError NotChronological

  -- A batch of log events in a single request cannot span more than 24 hours
  let
    minHour = NE.head neEvents ^. inputLogEvent_timestamp . to msToHours
    maxHour = NE.last neEvents ^. inputLogEvent_timestamp . to msToHours

  when ((maxHour - minHour) > spanHoursLimit)
    $ dispute
    $ logEventError SpansTooFar

  -- The maximum number of log events in a batch is 10,000
  when (length neEvents > eventsLimit)
    $ dispute
    $ logEventError TooManyEvents

  neEvents <$ traverse_ (validateLogEvent now) neEvents

validateLogEvent
  :: MonadValidate LogEventErrors m
  => UTCTime
  -> InputLogEvent
  -> m ()
validateLogEvent now event = do
  -- None of the log events in the batch can be more than 2 hours in the future
  when (timestamp `diffUTCTime` now > hours futureHoursLimit)
    $ dispute
    $ logEventError
    $ EventTooNew event

  -- None of the log events in the batch can be more than 14 days in the past
  when (now `diffUTCTime` timestamp > days pastDaysLimit)
    $ dispute
    $ logEventError
    $ EventTooOld event

  -- Message cannot be empty
  when (T.null message)
    $ dispute
    $ logEventError
    $ EventEmpty event

  -- Message cannot be more than 10,000 characters
  when (T.length message > messageLimit)
    $ dispute
    $ logEventError
    $ EventEmpty event
 where
  timestamp = event ^. inputLogEvent_timestamp . to msToTime
  message = event ^. inputLogEvent_message

refuteRejectedEvents
  :: MonadValidate LogEventErrors m
  => NonEmpty InputLogEvent
  -> RejectedLogEventsInfo
  -> m ()
refuteRejectedEvents _ _ = pure ()

msToHours :: Natural -> Double
msToHours = (/ 3600000) . realToFrac

msToTime :: Natural -> UTCTime
msToTime = posixSecondsToUTCTime . (/ 1000) . realToFrac

hours :: Int -> NominalDiffTime
hours n = fromIntegral n * 60 * 60

days :: Int -> NominalDiffTime
days n = hours $ n * 24

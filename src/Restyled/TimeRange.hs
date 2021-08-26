module Restyled.TimeRange
    ( TimeRange
    , timeRangeBefore
    , timeRangeFromAgo
    , withinTimeRange
    ) where

import Restyled.Prelude.Esqueleto

import Restyled.Time

data TimeRange = TimeRange
    { tmFrom :: UTCTime
    , tmTo :: UTCTime
    }

instance ToJSON TimeRange where
    toJSON TimeRange {..} = object ["from" .= tmFrom, "to" .= tmTo]

timeRangeBefore :: TimeRange -> TimeRange
timeRangeBefore TimeRange {..} = TimeRange
    { tmFrom = addUTCTime (negate diff) tmFrom
    , tmTo = tmFrom
    }
    where diff = diffUTCTime tmTo tmFrom

timeRangeFromAgo :: (MonadIO m, HasSeconds t) => t -> m TimeRange
timeRangeFromAgo t = do
    now <- getCurrentTime
    pure $ timeRangeToVia now $ subtractTime t

timeRangeToVia :: UTCTime -> (UTCTime -> UTCTime) -> TimeRange
timeRangeToVia tmTo f = let tmFrom = f tmTo in TimeRange { .. }

withinTimeRange :: SqlExpr (Value UTCTime) -> TimeRange -> SqlExpr (Value Bool)
withinTimeRange field TimeRange {..} =
    field >=. val tmFrom &&. field <=. val tmTo

infix 4 `withinTimeRange`

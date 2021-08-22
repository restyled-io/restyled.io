module Restyled.TimeRange
    ( TimeRange
    , timeRangeBefore
    , timeRangeFromAgo
    , withinTimeRange
    , timeRangeFilters
    ) where

import Restyled.Prelude

import qualified Database.Esqueleto as E
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

withinTimeRange
    :: E.SqlExpr (E.Value UTCTime) -> TimeRange -> E.SqlExpr (E.Value Bool)
withinTimeRange field TimeRange {..} =
    field E.>=. E.val tmFrom E.&&. field E.<=. E.val tmTo

infix 4 `withinTimeRange`

timeRangeFilters :: EntityField e UTCTime -> TimeRange -> [Filter e]
timeRangeFilters field TimeRange {..} = [field >=. tmFrom, field <=. tmTo]

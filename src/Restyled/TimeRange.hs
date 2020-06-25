module Restyled.TimeRange
    ( TimeRange

    -- * Construction
    -- ** Relative to now
    , timeRangeFromMinutesAgo

    -- ** From Query params
    , requiredTimeRange

    -- * Querying
    , rawSqlWithTimeRange
    , selectListWithTimeRange
    , selectListWithTimeRangeBy
    , timeRangeFilter
    )
where

import Restyled.Prelude

import Database.Persist.Sql (RawSql, rawSql)
import Restyled.Yesod

data TimeRange = TimeRange
    { tmFrom :: UTCTime
    , tmTo :: UTCTime
    }

timeRangeFromMinutesAgo :: MonadIO m => Int -> m TimeRange
timeRangeFromMinutesAgo minutes = do
    now <- getCurrentTime
    pure $ timeRangeToVia now $ subtractMinutes minutes

timeRangeToVia :: UTCTime -> (UTCTime -> UTCTime) -> TimeRange
timeRangeToVia tmTo f = let tmFrom = f tmTo in TimeRange { .. }

subtractMinutes :: Int -> UTCTime -> UTCTime
subtractMinutes minutes = addUTCTime $ fromIntegral $ negate $ minutes * 60

-- brittany-disable-next-binding

requiredTimeRange
    :: ( RenderMessage (HandlerSite m) FormMessage
       , MonadHandler m
       )
    => m TimeRange
requiredTimeRange = runInputGet $ TimeRange
    <$> ireq epochField "from"
    <*> ireq epochField "to"

rawSqlWithTimeRange
    :: (RawSql a, MonadIO m) => TimeRange -> Text -> SqlPersistT m [a]
rawSqlWithTimeRange TimeRange {..} =
    flip rawSql [PersistUTCTime tmFrom, PersistUTCTime tmTo]

selectListWithTimeRange
    :: (MonadIO m, SqlEntity a)
    => EntityField a UTCTime
    -> TimeRange
    -> SqlPersistT m [Entity a]
selectListWithTimeRange = selectListWithTimeRangeBy id

selectListWithTimeRangeBy
    :: (MonadIO m, SqlEntity a, PersistField b)
    => (UTCTime -> b)
    -- ^ How to turn the range time into the entity field type
    -> EntityField a b
    -- ^ The entity field to filter with the range
    -> TimeRange
    -> SqlPersistT m [Entity a]
selectListWithTimeRangeBy f field range =
    selectList (timeRangeFilterBy f field range) [Desc field]

timeRangeFilter :: EntityField a UTCTime -> TimeRange -> [Filter a]
timeRangeFilter = timeRangeFilterBy id

timeRangeFilterBy
    :: PersistField b
    => (UTCTime -> b)
    -> EntityField a b
    -> TimeRange
    -> [Filter a]
timeRangeFilterBy f field TimeRange {..} =
    [field >=. f tmFrom, field <=. f tmTo]

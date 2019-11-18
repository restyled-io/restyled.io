module Restyled.TimeRange
    ( TimeRange

    -- * Construction
    , timeRangeFromMinutesAgo
    , timeRangeFromVia
    , timeRangeToVia
    , subRangesToByMinutes
    , subRangeToByMinutes

    -- ** Params
    , requiredTimeRange

    -- * Querying
    , rawSqlWithTimeRange
    , selectListWithTimeRange
    , selectListWithTimeRange'
    , filterTimeRange
    )
where

import Restyled.Prelude

import Database.Persist.Sql (RawSql, SqlPersistT, rawSql)
import Restyled.Yesod

data TimeRange = TimeRange
    { tmFrom :: UTCTime
    , tmTo :: UTCTime
    }

timeRangeFromMinutesAgo :: MonadIO m => Int -> m TimeRange
timeRangeFromMinutesAgo minutes = do
    now <- getCurrentTime
    pure $ timeRangeToVia now $ subtractMinutes minutes

timeRangeFromVia :: UTCTime -> (UTCTime -> UTCTime) -> TimeRange
timeRangeFromVia tmFrom f = let tmTo = f tmFrom in TimeRange { .. }

timeRangeToVia :: UTCTime -> (UTCTime -> UTCTime) -> TimeRange
timeRangeToVia tmTo f = let tmFrom = f tmTo in TimeRange { .. }

subRangesToByMinutes :: TimeRange -> [Int] -> [TimeRange]
subRangesToByMinutes tm = map $ subRangeToByMinutes tm

subRangeToByMinutes :: TimeRange -> Int -> TimeRange
subRangeToByMinutes TimeRange {..} = timeRangeToVia tmTo . subtractMinutes

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
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => EntityField a (Maybe UTCTime)
    -> TimeRange
    -> SqlPersistT m [Entity a]
selectListWithTimeRange field TimeRange {..} =
    selectList [field >=. Just tmFrom, field <=. Just tmTo] [Desc field]

-- | @'selectListWithTimeRange'@ for a non-@'Maybe'@ field
selectListWithTimeRange'
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => EntityField a UTCTime
    -> TimeRange
    -> SqlPersistT m [Entity a]
selectListWithTimeRange' field TimeRange {..} =
    selectList [field >=. tmFrom, field <=. tmTo] [Desc field]

filterTimeRange :: (a -> UTCTime) -> TimeRange -> [a] -> [a]
filterTimeRange f TimeRange {..} = filter (inRange . f)
  where
    inRange x
        | x < tmFrom = False
        | x > tmTo = False
        | otherwise = True

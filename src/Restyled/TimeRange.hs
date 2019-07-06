module Restyled.TimeRange
    ( TimeRange
    , requiredTimeRange
    , rawSqlWithTimeRange
    , selectListWithTimeRange
    )
where

import Restyled.Prelude

import Database.Persist.Sql (RawSql, SqlPersistT, rawSql)
import Restyled.Yesod

data TimeRange = TimeRange
    { tmFrom :: UTCTime
    , tmTo :: UTCTime
    }

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

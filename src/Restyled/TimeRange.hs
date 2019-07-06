module Restyled.TimeRange
    ( TimeRange
    , requiredTimeRange
    , rawSqlWithTimeRange
    )
where

import Restyled.Prelude

import Database.Persist.Sql (RawSql, SqlPersistT, rawSql)
import Restyled.Yesod

data TimeRange = TimeRange
    { tmFrom :: UTCTime
    , tmTo :: UTCTime
    }

rawSqlWithTimeRange
    :: (RawSql a, MonadIO m) => TimeRange -> Text -> SqlPersistT m [a]
rawSqlWithTimeRange TimeRange {..} =
    flip rawSql [PersistUTCTime tmFrom, PersistUTCTime tmTo]

-- brittany-disable-next-binding

requiredTimeRange
    :: ( RenderMessage (HandlerSite m) FormMessage
       , MonadHandler m
       )
    => m TimeRange
requiredTimeRange = runInputGet $ TimeRange
    <$> ireq epochField "from"
    <*> ireq epochField "to"

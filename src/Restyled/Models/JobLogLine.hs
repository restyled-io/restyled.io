module Restyled.Models.JobLogLine
    ( fetchJobLogLines
    , streamJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Restyled.Models.DB

fetchJobLogLines
    :: MonadIO m => JobId -> Maybe UTCTime -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId mSince = selectList
    (catMaybes
        [Just (JobLogLineJob ==. jobId), (JobLogLineCreatedAt >.) <$> mSince]
    )
    [Asc JobLogLineCreatedAt]

streamJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
    => JobId
    -> Maybe UTCTime
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince = do
    jobLogLines <- lift $ runDB $ entityVal <$$> fetchJobLogLines jobId mSince
    yield jobLogLines

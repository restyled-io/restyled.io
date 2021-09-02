module Restyled.Models.JobLogLine
    ( fetchJobLogLines
    ) where

import Restyled.Prelude

import Restyled.Models.DB

fetchJobLogLines
    :: MonadIO m => JobId -> Maybe UTCTime -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId mSince = selectList
    (catMaybes
        [Just (JobLogLineJob ==. jobId), (JobLogLineCreatedAt >.) <$> mSince]
    )
    [Asc JobLogLineCreatedAt]

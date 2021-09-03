module Restyled.Models.JobLogLine
    ( fetchJobLogLines
    , streamJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Data.Conduit.List (chunksOf)
import Restyled.Models.DB

fetchJobLogLines
    :: MonadIO m
    => JobId
    -> Maybe UTCTime
    -> Maybe Natural
    -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId mSince mLimit = selectList
    (catMaybes
        [Just (JobLogLineJob ==. jobId), (JobLogLineCreatedAt >.) <$> mSince]
    )
    (catMaybes
        [Just $ Asc JobLogLineCreatedAt, LimitTo . fromIntegral <$> mLimit]
    )

streamJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
    => JobId
    -> Maybe UTCTime
    -> Maybe Natural
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince mPageSize = do
    jobLogLines <-
        lift $ runDB $ entityVal <$$> fetchJobLogLines jobId mSince Nothing

    case mPageSize of
        Nothing -> yield jobLogLines
        Just ps -> yieldMany jobLogLines .| chunksOf (fromIntegral ps)

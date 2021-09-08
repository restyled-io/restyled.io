module Restyled.Models.JobLogLine
    ( streamJobLogLines

    -- * Exported only for deprecated 'completeJob'
    , fetchJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import Restyled.Models.DB

streamJobLogLines
    :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
    => JobId
    -> Maybe UTCTime
    -> ConduitT () [JobLogLine] m ()
streamJobLogLines jobId mSince = do
    jobLogLines <- lift $ runDB $ do
        mCompressed <- fetchCompressedJobLog jobId mSince
        maybe (entityVal <$$> fetchJobLogLines jobId mSince) pure mCompressed

    yield jobLogLines

fetchCompressedJobLog
    :: MonadIO m => JobId -> Maybe UTCTime -> SqlPersistT m (Maybe [JobLogLine])
fetchCompressedJobLog jobId mSince = runMaybeT $ do
    job <- getT jobId
    jsonb <- hoistMaybe $ jobLog job
    pure
        $ maybe id (\s -> dropWhile ((<= s) . jobLogLineCreatedAt)) mSince
        $ sortOn jobLogLineCreatedAt
        $ map entityVal
        $ unJSONB jsonb

fetchJobLogLines
    :: MonadIO m => JobId -> Maybe UTCTime -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId mSince = selectList
    (catMaybes
        [Just (JobLogLineJob ==. jobId), (JobLogLineCreatedAt >.) <$> mSince]
    )
    [Asc JobLogLineCreatedAt]

{-# LANGUAGE TupleSections #-}

module Restyled.Models.Job
    (
    -- * Queries
      fetchJobIsInProgress
    , fetchJobLogLines

    -- * @'JobOutput'@
    , JobOutput(..)
    , attachJobOutput
    , fetchJobLog
    , fetchJobOutput
    ) where

import Restyled.Prelude

import Restyled.Models.DB

fetchJobIsInProgress :: MonadIO m => JobId -> SqlPersistT m Bool
fetchJobIsInProgress jobId =
    isJust <$> selectFirst [JobId ==. jobId, JobCompletedAt ==. Nothing] []

fetchJobLog :: MonadIO m => Entity Job -> SqlPersistT m [Entity JobLogLine]
fetchJobLog (Entity jobId Job {..}) =
    maybe (fetchJobLogLines jobId 0) (pure . unJSONB) jobLog

fetchJobLogLines
    :: MonadIO m
    => JobId
    -> Int -- ^ Offset
    -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId offset = selectList
    [JobLogLineJob ==. jobId]
    [Asc JobLogLineCreatedAt, OffsetBy offset]

data JobOutput
    = JobOutputInProgress (Entity Job)
    | JobOutputCompleted [JobLogLine]
    | JobOutputCompressed Job

attachJobOutput
    :: MonadIO m => Entity Job -> SqlPersistT m (Entity Job, JobOutput)
attachJobOutput job = (job, ) <$> fetchJobOutput job

fetchJobOutput :: MonadIO m => Entity Job -> SqlPersistT m JobOutput
fetchJobOutput jobE@(Entity jobId job@Job {..}) =
    case (jobCompletedAt, jobLog, jobStdout, jobStderr) of
        -- Job is done and Log records (still) exist
        (Just _, Nothing, Nothing, Nothing) ->
            JobOutputCompleted . map entityVal <$> selectList
                [JobLogLineJob ==. jobId]
                [Asc JobLogLineCreatedAt]

        -- Job is done and Log has been written into the Job itself
        (Just _, Just (JSONB logLines), _, _) ->
            pure $ JobOutputCompleted $ map entityVal logLines

        -- Deprecated: old style compressed log
        (Just _, _, _, _) -> pure $ JobOutputCompressed job

        -- Job is in progress
        (Nothing, _, _, _) -> pure $ JobOutputInProgress jobE

{-# LANGUAGE LambdaCase #-}

module Restyled.Models.Job
    (
    -- * Formatting
      jobPath
    , jobOutcome

    -- * Creating Jobs
    , insertJob

    -- * Queries
    , fetchJobIsInProgress
    , fetchJobLogLines

    -- * @'JobOutput'@
    , JobOutput(..)
    , attachJobOutput
    , fetchJobLog
    , fetchJobOutput
    , captureJobLogLine
    , fetchLastJobLogLineCreatedAt
    , compressJobOutput

    -- * Completing Jobs
    , completeJobSkipped
    , completeJobErrored
    , completeJob
    )
where

import Restyled.Prelude

import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Time (diff)
import Restyled.Models.DB
import Restyled.Models.Repo (repoPullPath)

jobPath :: Entity Job -> Text
jobPath (Entity _ Job {..}) = repoPullPath jobOwner jobRepo jobPullRequest

jobOutcome :: Job -> Text
jobOutcome Job {..} = fromMaybe "N/A" $ do
    ec <- jobExitCode
    duration <- getDuration <$> jobCompletedAt
    pure $ pack $ "exited " <> show ec <> " in " <> duration
  where
    getDuration = TL.unpack . format (diff False) . diffUTCTime jobCreatedAt

insertJob
    :: MonadIO m => Entity Repo -> PullRequestNum -> SqlPersistT m (Entity Job)
insertJob (Entity _ Repo {..}) pullRequestNumber = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobSvcs = repoSvcs
        , jobOwner = repoOwner
        , jobRepo = repoName
        , jobPullRequest = pullRequestNumber
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobLog = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

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
    | JobOutputCompleted [Entity JobLogLine]
    | JobOutputCompressed Job

attachJobOutput
    :: MonadIO m => Entity Job -> SqlPersistT m (Entity Job, JobOutput)
attachJobOutput job = (job, ) <$> fetchJobOutput job

fetchJobOutput :: MonadIO m => Entity Job -> SqlPersistT m JobOutput
fetchJobOutput jobE@(Entity jobId job@Job {..}) =
    case (jobCompletedAt, jobLog, jobStdout, jobStderr) of
        -- Job is done and Log records (still) exist
        (Just _, Nothing, Nothing, Nothing) ->
            JobOutputCompleted <$> selectList
                [JobLogLineJob ==. jobId]
                [Asc JobLogLineCreatedAt]

        -- Job is done and Log has been written into the Job itself
        (Just _, Just (JSONB logLines), _, _) ->
            pure $ JobOutputCompleted logLines

        -- Deprecated: old style compressed log
        (Just _, _, _, _) -> pure $ JobOutputCompressed job

        -- Job is in progress
        (Nothing, _, _, _) -> pure $ JobOutputInProgress jobE

captureJobLogLine :: MonadIO m => JobId -> Text -> Text -> SqlPersistT m ()
captureJobLogLine jobId stream content = do
    now <- liftIO getCurrentTime
    insert_ JobLogLine
        { jobLogLineJob = jobId
        , jobLogLineCreatedAt = now
        , jobLogLineStream = stream
        , jobLogLineContent = content
        }

fetchLastJobLogLineCreatedAt
    :: MonadIO m => JobId -> SqlPersistT m (Maybe UTCTime)
fetchLastJobLogLineCreatedAt jobId =
    jobLogLineCreatedAt . entityVal <$$> selectFirst
        [JobLogLineJob ==. jobId]
        [Desc JobLogLineCreatedAt]

compressJobOutput :: MonadIO m => JobId -> SqlPersistT m ()
compressJobOutput jobId = do
    logLines <- fetchJobLogLines jobId 0
    update jobId [JobLog =. Just (JSONB logLines)]
    deleteWhere [JobLogLineJob ==. jobId]

completeJobSkipped
    :: MonadIO m => String -> Entity Job -> SqlPersistT m (Entity Job)
completeJobSkipped reason job = do
    captureJobLogLine (entityKey job) "system" $ pack reason
    completeJob ExitSuccess job

completeJobErrored
    :: MonadIO m => String -> Entity Job -> SqlPersistT m (Entity Job)
completeJobErrored reason job = do
    captureJobLogLine (entityKey job) "system" $ pack reason
    completeJob (ExitFailure 99) job

completeJob :: MonadIO m => ExitCode -> Entity Job -> SqlPersistT m (Entity Job)
completeJob ec job@(Entity jobId _) = do
    now <- liftIO getCurrentTime
    logLines <- fetchJobLogLines jobId 0
    updatedJob <- replaceEntity $ overEntity job $ \j -> j
        { jobUpdatedAt = now
        , jobCompletedAt = Just now
        , jobExitCode = Just $ exitCode ec
        , jobLog = Just $ JSONB logLines
        }
    updatedJob <$ deleteWhere [JobLogLineJob ==. jobId]

exitCode :: ExitCode -> Int
exitCode = \case
    ExitSuccess -> 0
    ExitFailure i -> i

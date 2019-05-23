{-# LANGUAGE LambdaCase #-}

module Models.Job
    (
    -- * Creating Jobs
      insertJob
    , insertJobRetry

    -- * Queries
    , fetchJobIsInProgress
    , fetchJobLogLines

    -- * @'JobOutput'@
    , JobOutput(..)
    , attachJobOutput
    , fetchJobOutput
    , captureJobLogLine

    -- * Completing Jobs
    , completeJobSkipped
    , completeJobErrored
    , completeJob
    )
where

import Restyled.Prelude

import Models.DB

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
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

insertJobRetry :: MonadIO m => Job -> SqlPersistT m (Entity Job)
insertJobRetry job = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobSvcs = jobSvcs job
        , jobOwner = jobOwner job
        , jobRepo = jobRepo job
        , jobPullRequest = jobPullRequest job
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

fetchJobIsInProgress :: MonadIO m => JobId -> SqlPersistT m Bool
fetchJobIsInProgress jobId =
    isJust <$> selectFirst [JobId ==. jobId, JobCompletedAt ==. Nothing] []

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
    | JobOutputLegacy Job

attachJobOutput
    :: MonadIO m => Entity Job -> SqlPersistT m (Entity Job, JobOutput)
attachJobOutput job = (job, ) <$> fetchJobOutput job

fetchJobOutput :: MonadIO m => Entity Job -> SqlPersistT m JobOutput
fetchJobOutput jobE@(Entity jobId job@Job {..}) =
    case (jobCompletedAt, jobStdout, jobStderr) of
        (Just _, Nothing, Nothing) -> JobOutputCompleted <$> selectList
            [JobLogLineJob ==. jobId]
            [Asc JobLogLineCreatedAt]
        (Just _, _, _) -> pure $ JobOutputLegacy job
        (_, _, _) -> pure $ JobOutputInProgress jobE

captureJobLogLine :: MonadIO m => JobId -> Text -> Text -> SqlPersistT m ()
captureJobLogLine jobId stream content = do
    now <- liftIO getCurrentTime
    insert_ JobLogLine
        { jobLogLineJob = jobId
        , jobLogLineCreatedAt = now
        , jobLogLineStream = stream
        , jobLogLineContent = content
        }

completeJobSkipped :: MonadIO m => String -> Entity Job -> SqlPersistT m ()
completeJobSkipped reason job = do
    captureJobLogLine (entityKey job) "system" $ pack reason
    completeJob ExitSuccess job

completeJobErrored :: MonadIO m => String -> Entity Job -> SqlPersistT m ()
completeJobErrored reason job = do
    captureJobLogLine (entityKey job) "system" $ pack reason
    completeJob (ExitFailure 99) job

completeJob :: MonadIO m => ExitCode -> Entity Job -> SqlPersistT m ()
completeJob ec job = do
    now <- liftIO getCurrentTime
    replaceEntity $ overEntity job $ \j -> j
        { jobUpdatedAt = now
        , jobCompletedAt = Just now
        , jobExitCode = Just $ exitCode ec
        }

exitCode :: ExitCode -> Int
exitCode = \case
    ExitSuccess -> 0
    ExitFailure i -> i

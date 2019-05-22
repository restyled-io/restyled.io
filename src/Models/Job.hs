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
    , completeJob
    , completeJobErrored
    , completeJobErroredS
    , completeJobSkipped
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

captureJobLogLine :: HasDB env => JobId -> Text -> Text -> RIO env ()
captureJobLogLine jobId stream content = runDB $ do
    now <- liftIO getCurrentTime
    insert_ JobLogLine
        { jobLogLineJob = jobId
        , jobLogLineCreatedAt = now
        , jobLogLineStream = stream
        , jobLogLineContent = content
        }

completeJob :: UTCTime -> ExitCode -> Job -> Job
completeJob now ec job = job
    { jobUpdatedAt = now
    , jobCompletedAt = Just now
    , jobExitCode = Just $ toInt ec
    }
  where
    toInt ExitSuccess = 0
    toInt (ExitFailure i) = i

--------------------------------------------------------------------------------
-- NB, below here is only useful for artificial failure/skip. Normally, output
-- is read via job_log_line records.
--------------------------------------------------------------------------------

completeJobErrored :: UTCTime -> String -> Job -> Job
completeJobErrored now reason job = (completeJob now (ExitFailure 99) job)
    { jobStdout = Just ""
    , jobStderr = Just $ pack reason
    }

completeJobErroredS :: Show a => UTCTime -> a -> Job -> Job
completeJobErroredS now = completeJobErrored now . show

completeJobSkipped :: UTCTime -> String -> Job -> Job
completeJobSkipped now reason job = (completeJob now ExitSuccess job)
    { jobStdout = Just $ pack reason
    , jobStderr = Just ""
    }

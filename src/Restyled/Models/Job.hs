{-# LANGUAGE TupleSections #-}

module Restyled.Models.Job
    (
    -- * Formatting
      jobOutcome

    -- * Creating Jobs
    , insertJob

    -- * Queries
    , fetchJobIsInProgress

    -- * @'JobOutput'@
    , JobOutput(..)
    , attachJobOutput
    , fetchJobOutput

    -- * Completing Jobs
    , completeJobSkipped
    , completeJobErrored
    , completeJob
    ) where

import Restyled.Prelude

import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Time (diff)
import Restyled.JobLogLine
import Restyled.Models.DB
import Restyled.Settings

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
        , jobStdout = Just "__cw"
        , jobStderr = Nothing
        }

fetchJobIsInProgress :: MonadIO m => JobId -> SqlPersistT m Bool
fetchJobIsInProgress jobId =
    isJust <$> selectFirst [JobId ==. jobId, JobCompletedAt ==. Nothing] []

data JobOutput
    = JobOutputInProgress (Entity Job)
    | JobOutputCompleted [JobLogLine]
    | JobOutputCompressed Job

expiredJobOutput :: MonadIO m => JobId -> m JobOutput
expiredJobOutput jobId = JobOutputCompleted . pure <$> jobLogLine
    jobId
    JobLogStreamSystem
    "Job log expired"

attachJobOutput
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => Entity Job
    -> SqlPersistT m (Entity Job, JobOutput)
attachJobOutput job = (job, ) <$> fetchJobOutput job

fetchJobOutput
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => Entity Job
    -> SqlPersistT m JobOutput
fetchJobOutput jobE@(Entity jobId job@Job {..}) =
    case (jobCompletedAt, jobLog, jobStdout, jobStderr) of
        -- Job is done and CW Logged
        (Just _, _, Just "__cw", _) -> do
            logLines <- lift $ fetchJobLogLines jobId
            if null logLines
                then expiredJobOutput jobId
                else pure $ JobOutputCompleted logLines

        -- Deprecated: Job is done and legacy Log records exist
        (Just _, Nothing, Nothing, Nothing) ->
            JobOutputCompleted . map entityVal <$> selectList
                [JobLogLineJob ==. jobId]
                [Asc JobLogLineCreatedAt]

        -- Deprecated: Job is done and legacy Log has been written
        (Just _, Just (JSONB logLines), _, _) ->
            pure $ JobOutputCompleted $ map entityVal logLines

        -- Deprecated: old style compressed log
        (Just _, _, _, _) -> pure $ JobOutputCompressed job

        -- Job is in progress
        (Nothing, _, _, _) -> pure $ JobOutputInProgress jobE

completeJobSkipped
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
    => String
    -> Entity Job
    -> SqlPersistT m (Entity Job)
completeJobSkipped reason job = do
    lift $ captureJobLogLine (entityKey job) JobLogStreamSystem $ pack reason
    completeJob ExitSuccess job

completeJobErrored
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
    => String
    -> Entity Job
    -> SqlPersistT m (Entity Job)
completeJobErrored reason job = do
    lift $ captureJobLogLine (entityKey job) JobLogStreamSystem $ pack reason
    completeJob (ExitFailure 99) job

completeJob
    :: MonadIO m => ExitCode -> Entity Job -> SqlPersistT m (Entity Job)
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

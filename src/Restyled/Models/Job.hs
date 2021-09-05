module Restyled.Models.Job
    (
    -- * Formatting
      jobOutcome

    -- * Creating Jobs
    , insertJob

    -- * @'JobOutput'@
    , captureJobLogLine
    , fetchLastJobLogLineCreatedAt

    -- * Completing Jobs
    , completeJobSkipped
    , completeJobErrored
    , completeJob

    -- * Temporary log-transition helpers
    , markJobAsCloudWatch
    , jobIsCloudWatch
    ) where

import Restyled.Prelude

import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Time (diff)
import Restyled.Models.DB
import Restyled.Models.JobLogLine

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

completeJob
    :: MonadIO m => ExitCode -> Entity Job -> SqlPersistT m (Entity Job)
completeJob ec job@(Entity jobId _) = do
    now <- liftIO getCurrentTime
    logLines <- fetchJobLogLines jobId Nothing
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

markJobAsCloudWatch :: Job -> Job
markJobAsCloudWatch job = job { jobStdout = Just cwSigil }

jobIsCloudWatch :: Job -> Bool
jobIsCloudWatch = (== Just cwSigil) . jobStdout

cwSigil :: Text
cwSigil = "__cw"

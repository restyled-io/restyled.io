{-# LANGUAGE RecordWildCards #-}

module Model.Job
    ( insertJob
    , insertJobRetry
    , completeJob
    , completeJobErrored
    , completeJobSkipped
    )
where

import ClassyPrelude

import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model
import System.Exit (ExitCode(..))

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

completeJob :: UTCTime -> (ExitCode, String, String) -> Job -> Job
completeJob now (ec, out, err) job = job
    { jobUpdatedAt = now
    , jobCompletedAt = Just now
    , jobExitCode = Just $ toInt ec
    , jobStdout = Just $ pack out
    , jobStderr = Just $ pack err
    }
  where
    toInt ExitSuccess = 0
    toInt (ExitFailure i) = i

completeJobErrored :: UTCTime -> SomeException -> Job -> Job
completeJobErrored now ex = completeJob now (ExitFailure 99, "", show ex)

completeJobSkipped :: UTCTime -> String -> Job -> Job
completeJobSkipped now reason = completeJob now (ExitSuccess, reason, "")

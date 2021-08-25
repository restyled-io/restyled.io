{-# LANGUAGE DuplicateRecordFields #-}

module Restyled.Api.CreateJob
    ( ApiCreateJob(..)
    , ApiCreateJobErrors
    , repoNotFound
    , createJob
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.Job (ApiJob, apiJob)
import Restyled.Models.DB
import Restyled.Settings

data ApiCreateJob = ApiCreateJob
    { owner :: OwnerName
    , repo :: RepoName
    , pullRequest :: PullRequestNum
    , completedAt :: Maybe UTCTime
    , exitCode :: Maybe Int
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass FromJSON

newtype ApiCreateJobErrors = ApiCreateJobErrors
    { errors :: NonEmpty ApiCreateJobError
    }
    deriving stock (Generic, Eq, Show)
    deriving newtype Semigroup
    deriving anyclass ToJSON

data ApiCreateJobError
    = RepoNotFound RepoNotFoundContents
    | Unused -- ^ Trick Aeson's Generic into tagging
    deriving stock (Generic, Eq, Show)
    deriving anyclass ToJSON

data RepoNotFoundContents = RepoNotFoundContents
    { owner :: OwnerName
    , name :: RepoName
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass ToJSON

repoNotFound :: OwnerName -> RepoName -> ApiCreateJobErrors
repoNotFound owner name =
    ApiCreateJobErrors $ pure $ RepoNotFound RepoNotFoundContents { .. }

createJob
    :: (MonadIO m, MonadReader env m, HasSettings env)
    => ApiCreateJob
    -> ValidateT ApiCreateJobErrors (SqlPersistT m) ApiJob
createJob ApiCreateJob {..} = do
    settings <- lift $ lift $ view settingsL
    mRepo <- lift $ getBy $ UniqueRepo svcs owner repo
    void $ refuteNothing (repoNotFound owner repo) mRepo

    now <- liftIO getCurrentTime
    job <- lift $ insertEntity Job
        { jobSvcs = svcs
        , jobOwner = owner
        , jobRepo = repo
        , jobPullRequest = pullRequest
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = completedAt
        , jobExitCode = exitCode

        -- Mark this as created with AWS Logs
        , jobStdout = Just "__cw"

        -- Legacy fields
        , jobLog = Nothing
        , jobStderr = Nothing
        }
    pure $ apiJob job settings
    where svcs = GitHubSVCS

refuteNothing :: MonadValidate e m => e -> Maybe a -> m a
refuteNothing e = maybe (refute e) pure

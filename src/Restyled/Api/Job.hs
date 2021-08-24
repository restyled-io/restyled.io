module Restyled.Api.Job
    ( ApiJob(..)
    , apiJob
    ) where

import Restyled.Prelude

import Restyled.Models.DB

data ApiJob = ApiJob
    { id :: JobId
    , owner :: OwnerName
    , repo :: RepoName
    , pullRequest :: PullRequestNum
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , completedAt :: Maybe UTCTime
    , exitCode :: Maybe Int
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass ToJSON

apiJob :: Entity Job -> ApiJob
apiJob (Entity jobId Job {..}) = ApiJob
    { id = jobId
    , owner = jobOwner
    , repo = jobRepo
    , pullRequest = jobPullRequest
    , createdAt = jobCreatedAt
    , updatedAt = jobUpdatedAt
    , completedAt = jobCompletedAt
    , exitCode = jobExitCode
    }

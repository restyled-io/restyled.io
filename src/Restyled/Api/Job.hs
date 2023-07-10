module Restyled.Api.Job
  ( ApiJob (..)
  , apiJob
  ) where

import Restyled.Prelude

import Restyled.Models.DB
import Restyled.Settings (AppSettings (..))

data ApiJob = ApiJob
  { id :: JobId
  , owner :: OwnerName
  , repo :: RepoName
  , pullRequest :: PullRequestNum
  , url :: Text
  , awsLogGroup :: Text
  , awsLogStream :: Text
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON)

apiJob :: Entity Job -> AppSettings -> ApiJob
apiJob (Entity jobId Job {..}) AppSettings {..} =
  ApiJob
    { id = jobId
    , owner = jobOwner
    , repo = jobRepo
    , pullRequest = jobPullRequest
    , url =
        appRoot
          <> "/gh/"
          <> toPathPiece jobOwner
          <> "/repos/"
          <> toPathPiece jobRepo
          <> "/jobs/"
          <> toPathPiece jobId
    , awsLogGroup = appRestylerLogGroup
    , awsLogStream = appRestylerLogStreamPrefix <> toPathPiece jobId
    }

module SVCS.Payload
    ( Payload(..)
    , PullRequestEventType(..)
    ) where

import ClassyPrelude

import GitHub.Data.PullRequests (PullRequestEventType(..))
import SVCS.Names

data Payload = Payload
    { pAction :: PullRequestEventType
    , pAuthor :: Text
    , pOwnerName :: OwnerName
    , pRepoName :: RepoName
    , pRepoIsPrivate :: Bool
    , pInstallationId :: InstallationId
    , pPullRequest :: PullRequestNum
    }
    deriving Show

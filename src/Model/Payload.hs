module Model.Payload
    ( Payload(..)
    , PullRequestEventType(..)
    , enqueueEvents
    ) where

import ClassyPrelude

import GitHub.Data.PullRequests (PullRequestEventType(..))
import Model.Names

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

enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized]

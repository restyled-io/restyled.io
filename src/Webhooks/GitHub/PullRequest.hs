{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Webhooks.GitHub.PullRequest
    ( Payload(..)
    , toOrganization
    , toRepository
    , toPullRequest
    ) where

import ClassyPrelude

import Data.Aeson
import Model hiding (Organization, Repository, PullRequest)
import qualified Model as M

data Action
    = Opened
    | Closed
    deriving (Eq, Show)

instance FromJSON Action where
    parseJSON = withText "PullRequest.Action" $ \case
        "opened" -> pure Opened
        "closed" -> pure Closed
        _ -> mzero

data Organization = Organization
    { oId :: GitHubId
    , oLogin :: Text
    }
    deriving (Eq, Show)

instance FromJSON Organization where
    parseJSON = withObject "PullRequest.Organization" $ \o -> Organization
        <$> o .: "id"
        <*> o .: "login"

data Repository = Repository
    { rId :: GitHubId
    , rFullName :: Text
    , rName :: Text
    , rIsPrivate :: Bool
    , rOwnerId :: GitHubId
    }
    deriving (Eq, Show)

instance FromJSON Repository where
    parseJSON = withObject "PullRequest.Repository" $ \o -> Repository
        <$> o .: "id"
        <*> o .: "full_name"
        <*> o .: "name"
        <*> o .: "private"
        <*> (o .: "owner" >>= (.: "id"))

data PullRequest = PullRequest
    { prNumber :: Int
    , prId :: GitHubId
    , prBaseSHA :: CommitSHA
    , prHeadSHA :: CommitSHA
    }
    deriving (Eq, Show)

instance FromJSON PullRequest where
    parseJSON = withObject "GitHub.PullRequest" $ \o -> PullRequest
        <$> o .: "number"
        <*> o .: "id"
        <*> (o .: "base" >>= (.: "sha"))
        <*> (o .: "head" >>= (.: "sha"))

newtype Sender = Sender Text
    deriving (Eq, Show)

instance FromJSON Sender where
    parseJSON = withObject "GitHub.Sender" $ \o -> Sender
        <$> o .: "login"

data Payload = Payload
    { pAction :: Action
    , pPullRequest :: PullRequest
    , pRepository :: Repository
    , pOrganization :: Organization
    , pSender :: Sender
    }
    deriving (Eq, Show)

instance FromJSON Payload where
    parseJSON = withObject "PullRequest.Payload" $ \o -> Payload
        <$> o .: "action"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> o .: "organization"
        <*> o .: "sender"

toOrganization :: Payload -> M.Organization
toOrganization Payload{..} = M.Organization
    { organizationGithubId = oId pOrganization
    , organizationGithubLogin = oLogin pOrganization
    }

toRepository :: OrganizationId -> Payload -> M.Repository
toRepository orgId Payload{..} = M.Repository
    { repositoryFullName = rFullName pRepository
    , repositoryIsPrivate = rIsPrivate pRepository
    , repositoryName = rName pRepository
    , repositoryGithubId = rId pRepository
    , repositoryGithubOwnerId = rOwnerId pRepository
    , repositoryOrganization = orgId
    }

toPullRequest :: RepositoryId -> Payload -> M.PullRequest
toPullRequest repoId Payload{..} = M.PullRequest
    { pullRequestNumber = prNumber pPullRequest
    , pullRequestGithubId = prId pPullRequest
    , pullRequestBaseCommitSHA = prBaseSHA pPullRequest
    , pullRequestHeadCommitSHA = prHeadSHA pPullRequest
    , pullRequestRepository = repoId
    }

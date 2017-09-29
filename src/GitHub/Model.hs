{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Model
    ( AccessToken(..)
    , Repository(..)
    , RepoRef(..)
    , PullRequest(..)
    , Comment(..)
    , module Model.Base
    ) where

import ClassyPrelude

import Data.Aeson
import Model.Base

data AccessToken = AccessToken
    { atToken :: Text
    , atExpiresAt :: UTCTime
    }
    deriving Show

instance FromJSON AccessToken where
    parseJSON = withObject "GitHub.AccessToken" $ \o -> AccessToken
        <$> o .: "token"
        <*> o .: "expires_at"

data Repository = Repository
    { rFullName :: RepoFullName
    }
    deriving Show

instance FromJSON Repository where
    parseJSON = withObject "GitHub.Repository" $ \o -> Repository
        <$> o .: "full_name"

data RepoRef = RepoRef
    { rrRepository :: Repository
    , rrRef :: Branch
    }
    deriving Show

instance FromJSON RepoRef where
    parseJSON = withObject "GitHub.RepoRef" $ \o -> RepoRef
        <$> o .: "repo"
        <*> o .: "ref"

data PullRequest = PullRequest
    { prNumber :: PRNumber
    , prTitle :: PRTitle
    , prBase :: RepoRef
    , prHead :: RepoRef
    }
    deriving Show

instance FromJSON PullRequest where
    parseJSON = withObject "GitHub.PullRequest" $ \o -> PullRequest
        <$> o .: "number"
        <*> o .: "title"
        <*> o .: "base"
        <*> o .: "head"

data Comment = Comment
    { cId :: GitHubId
    , cBody :: Text
    }
    deriving Show

instance FromJSON Comment where
    parseJSON = withObject "GitHub.Comment" $ \o -> Comment
        <$> o .: "id"
        <*> o .: "body"

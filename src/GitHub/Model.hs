{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GitHub.Model
    ( GitHubId(..)
    , AccessToken(..)
    , RepoFullName(..)
    , Repository(..)
    , Branch(..)
    , RepoRef(..)
    , PRNumber(..)
    , PRTitle(..)
    , PullRequest(..)
    , Comment(..)
    ) where

import ClassyPrelude.Yesod

import Data.Aeson
import Database.Persist.Sql

newtype GitHubId = GitHubId { unGitHubId :: Int } deriving
    ( Eq
    , FromJSON
    , PersistField
    , PersistFieldSql
    , PathPiece
    , Show
    , ToJSON
    )

data AccessToken = AccessToken
    { atToken :: Text
    , atExpiresAt :: UTCTime
    }
    deriving Show

instance FromJSON AccessToken where
    parseJSON = withObject "GitHub.AccessToken" $ \o -> AccessToken
        <$> o .: "token"
        <*> o .: "expires_at"

newtype RepoFullName = RepoFullName { unRepoFullName :: Text } deriving
    ( Eq
    , FromJSON
    , IsString
    , PathPiece
    , PersistField
    , PersistFieldSql
    , Show
    , ToJSON
    )

data Repository = Repository
    { rFullName :: RepoFullName
    }
    deriving Show

instance FromJSON Repository where
    parseJSON = withObject "GitHub.Repository" $ \o -> Repository
        <$> o .: "full_name"

instance ToJSON Repository where
    toJSON Repository{..} = object
        [ "full_name" .= rFullName
        ]

newtype Branch = Branch { unBranch :: Text } deriving
    ( Eq
    , FromJSON
    , IsString
    , PersistField
    , PersistFieldSql
    , Semigroup
    , Show
    , ToJSON
    )

data RepoRef = RepoRef
    { rrRepository :: Repository
    , rrRef :: Branch
    }
    deriving Show

instance FromJSON RepoRef where
    parseJSON = withObject "GitHub.RepoRef" $ \o -> RepoRef
        <$> o .: "repo"
        <*> o .: "ref"

instance ToJSON RepoRef where
    toJSON RepoRef{..} = object
        [ "repo" .= rrRepository
        , "ref" .= rrRef
        ]

newtype PRNumber = PRNumber { unPRNumber :: Int } deriving
    ( Eq
    , FromJSON
    , PathPiece
    , PersistField
    , PersistFieldSql
    , Show
    , ToJSON
    )

newtype PRTitle = PRTitle { unPRTitle :: Text } deriving
    ( Eq
    , FromJSON
    , IsString
    , PathPiece
    , PersistField
    , PersistFieldSql
    , Semigroup
    , Show
    , ToJSON
    )

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

instance ToJSON PullRequest where
    toJSON PullRequest{..} = object
        [ "number" .= prNumber
        , "title" .= prTitle
        , "base" .= prBase
        , "head" .= prHead
        ]

data Comment = Comment
    { cId :: GitHubId
    , cBody :: Text
    }
    deriving Show

instance FromJSON Comment where
    parseJSON = withObject "GitHub.Comment" $ \o -> Comment
        <$> o .: "id"
        <*> o .: "body"

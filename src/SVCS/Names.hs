{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SVCS.Names
    ( OwnerName
    , mkOwnerName
    , RepoName
    , mkRepoName
    , InstallationId
    , PullRequestNum
    , mkPullRequestNum
    , GitHubUserId
    , GitHubUserName
    , userToOwnerName
    , ownerToUserName
    , GitLabUserId
    , GitLabUserName
    , RepoAccessToken(..)
    , RepoSVCS(..)
    , showRepoSVCS
    ) where

import Prelude

import Control.Monad ((<=<))
import Data.Aeson
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Database.Persist.Sql
import GitHub.Data
import GitHub.Data.Apps
import Text.Blaze (ToMarkup(..))
import Text.Read hiding (String)
import Yesod.Core (PathPiece(..))

type OwnerName = Name Owner
type RepoName = Name Repo
type InstallationId = Id Installation

type PullRequestNum = Id PullRequest

mkPullRequestNum :: Int -> PullRequestNum
mkPullRequestNum = mkId Proxy

type GitHubUserId = Id User
type GitHubUserName = Name User

userToOwnerName :: GitHubUserName -> OwnerName
userToOwnerName = mkName Proxy . untagName

ownerToUserName :: OwnerName -> GitHubUserName
ownerToUserName = mkName Proxy . untagName

-- TODO: newtype for actual safety
type GitLabUserId = Id User
type GitLabUserName = Name User

newtype RepoAccessToken = RepoAccessToken { unRepoAccessToken :: Text }

data RepoSVCS = GitHubSVCS | GitLabSVCS
    deriving (Eq, Ord, Read, Show, Enum, Bounded)

readRepoSVCS :: Text -> Either Text RepoSVCS
readRepoSVCS = \case
    "github" -> Right GitHubSVCS
    "gitlab" -> Right GitLabSVCS
    x -> Left $ "Invalid SVCS value: " <> x

showRepoSVCS :: RepoSVCS -> Text
showRepoSVCS = \case
    GitHubSVCS -> "github"
    GitLabSVCS -> "gitlab"

instance PathPiece RepoSVCS where
    toPathPiece GitHubSVCS = "gh"
    toPathPiece GitLabSVCS = "gl"
    fromPathPiece "gh" = Just GitHubSVCS
    fromPathPiece "gl" = Just GitLabSVCS
    fromPathPiece _ = Nothing

instance PersistField RepoSVCS where
    toPersistValue = toPersistValue . showRepoSVCS
    fromPersistValue = readRepoSVCS <=< fromPersistValue

instance ToJSON RepoSVCS where
    toJSON = String . showRepoSVCS

instance FromJSON RepoSVCS where
    parseJSON = withText "SVCS" $ either (fail . unpack) pure . readRepoSVCS

instance PersistFieldSql RepoSVCS where
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance Read (Id a) where
    readPrec = mkId Proxy <$> readPrec

instance PathPiece (Id a) where
    toPathPiece = toPathPiece . untagId
    fromPathPiece = (mkId Proxy <$>) . fromPathPiece

instance PersistField (Id a) where
    toPersistValue = toPersistValue . untagId
    fromPersistValue = (mkId Proxy <$>) . fromPersistValue

instance PersistFieldSql (Id a) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

instance ToMarkup (Id a) where
    toMarkup = toMarkup . untagId

instance Read (Name a) where
    readsPrec n = map (\(x, s) -> (mkName Proxy x, s)) . readsPrec n

instance PathPiece (Name  a) where
    toPathPiece = toPathPiece . untagName
    fromPathPiece = (mkName Proxy <$>) . fromPathPiece

instance PersistField (Name a) where
    toPersistValue = toPersistValue . untagName
    fromPersistValue = (mkName Proxy <$>) . fromPersistValue

instance PersistFieldSql (Name a) where
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance ToMarkup (Name a) where
    toMarkup = toMarkup . untagName

instance Num (Id a) where
    fromInteger = mkId Proxy . fromInteger

    (+) = errSneakyNum
    (-) = errSneakyNum
    (*) = errSneakyNum
    abs = errSneakyNum
    signum = errSneakyNum

errSneakyNum :: a
errSneakyNum = error "Num instance only present for `fromInteger'"

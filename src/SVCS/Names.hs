{-# OPTIONS_GHC -fno-warn-orphans #-}

module SVCS.Names
    ( nameToName

    -- * Single-word aliases, for use in models and routes
    , OwnerName
    , RepoName
    , InstallationId

    -- * Disambiguate from our own types
    , GitHubAuth
    , GitHubAppId
    , GitHubAppKey
    , GitHubUserId
    , GitHubUserName

    -- * Deprecated
    -- ** Compatibility from when @github@ introduced @'IssueNumber'@
    , PullRequestNum

    -- ** GitLab
    , GitLabUserId
    , GitLabUserName
    , RepoSVCS(..)
    , showRepoSVCS

    -- ** Abortive attempt to encapsulate
    , mkOwnerName
    , mkRepoName
    , mkUserId
    , mkUserName
    ) where

import Prelude

import Control.Monad ((<=<))
import Data.Aeson
import Data.Bifunctor (first)
import Data.Csv (ToField(..))
import Data.Proxy
import Data.Text (Text, unpack)
import qualified Database.Esqueleto.Legacy as E
import Database.Persist.Sql
import GitHub.Data
import GitHub.Data.Apps
import GitHub.Data.Installations
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.Text ()
import Text.Blaze (ToMarkup(..))
import Text.Read hiding (String)
import Web.PathPieces

type OwnerName = Name Owner
type RepoName = Name Repo
type InstallationId = Id Installation

type GitHubAuth = Auth

type GitHubAppId = Id App
type GitHubAppKey = AppKey

type GitHubUserId = Id User
type GitHubUserName = Name User

type PullRequestNum = IssueNumber

nameToName :: Name a -> Name b
nameToName = mkName Proxy . untagName

type GitLabUserId = Id User
type GitLabUserName = Name User

data RepoSVCS = GitHubSVCS | GitLabSVCS
    deriving stock (Eq, Ord, Read, Show, Enum, Bounded)

instance Arbitrary RepoSVCS where
    arbitrary = pure GitHubSVCS

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

instance Arbitrary (Id a) where
    arbitrary = mkId Proxy <$> arbitrary

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

deriving newtype instance Arbitrary IssueNumber
deriving newtype instance Num IssueNumber
deriving newtype instance Read IssueNumber
deriving newtype instance PathPiece IssueNumber
deriving newtype instance PersistField IssueNumber
deriving newtype instance PersistFieldSql IssueNumber
deriving newtype instance ToMarkup IssueNumber

instance Arbitrary (Name a) where
    arbitrary = mkName Proxy <$> arbitrary

instance Read (Name a) where
    readsPrec n = map (first $ mkName Proxy) . readsPrec n

instance PathPiece (Name  a) where
    toPathPiece = toPathPiece . untagName
    fromPathPiece = (mkName Proxy <$>) . fromPathPiece

instance PersistField (Name a) where
    toPersistValue = toPersistValue . untagName
    fromPersistValue = (mkName Proxy <$>) . fromPersistValue

instance PersistFieldSql (Name a) where
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance E.SqlString (Name a)

instance ToMarkup (Name a) where
    toMarkup = toMarkup . untagName

instance ToField (Name a) where
    toField = toField . untagName

instance Num (Id a) where
    fromInteger = mkId Proxy . fromInteger

    (+) = errSneakyNum
    (-) = errSneakyNum
    (*) = errSneakyNum
    abs = errSneakyNum
    signum = errSneakyNum

errSneakyNum :: a
errSneakyNum = error "Num instance only present for `fromInteger'"

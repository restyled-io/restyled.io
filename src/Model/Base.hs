{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
--
-- N.B. This module only exists to work around the GHC stage restriction, and
-- define some datatypes we want to use in mkPersist in Model.hs
--
module Model.Base where

import ClassyPrelude.Yesod
import Database.Persist.Sql

data JobState = Created | InProgress | Done
    deriving (Eq, Read, Show)

derivePersistField "JobState"

newtype RepoName = RepoName { unRepoName :: Text } deriving
    ( Eq
    , FromJSON
    , IsString
    , PathPiece
    , PersistField
    , PersistFieldSql
    , Show
    , ToJSON
    )

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

newtype CommitSHA = CommitSHA { unCommitSHA :: Text } deriving
    ( Eq
    , FromJSON
    , IsString
    , PersistField
    , PersistFieldSql
    , Show
    , ToJSON
    )

newtype GitHubId = GitHubId { unGitHubId :: Int } deriving
    ( Eq
    , FromJSON
    , PersistField
    , PersistFieldSql
    , PathPiece
    , Show
    , ToJSON
    )

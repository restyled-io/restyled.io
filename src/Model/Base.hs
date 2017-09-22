{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
--
-- N.B. This module only exists to work around the GHC stage restriction, and
-- define some datatypes we want to use in mkPersist in Model.hs
--
module Model.Base where

import ClassyPrelude
import Data.Aeson
import Database.Persist.Sql

newtype CommitSHA = CommitSHA Text deriving
    ( Eq
    , FromJSON
    , PersistField
    , PersistFieldSql
    , Show
    )

newtype GitHubId = GitHubId Int deriving
    ( Eq
    , FromJSON
    , PersistField
    , PersistFieldSql
    , Show
    )

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
--
-- This module exists to define data types needed in persistent definitions.
--
module Model.Base where

import ClassyPrelude.Yesod hiding (Proxy)

import Database.Persist.Sql
import Data.Proxy
import GitHub.Data
import GitHub.Data.Apps

instance Num (Id a) where
    fromInteger = mkId Proxy . fromInteger

    (+) = errSneakyNum
    (-) = errSneakyNum
    (*) = errSneakyNum
    abs = errSneakyNum
    signum = errSneakyNum

errSneakyNum :: a
errSneakyNum = error $ unlines
    [ "I'm sorry, this is a GitHub Id, not really a number. We only have Num so"
    , " we can use literals, conversion functions, arbitrary etc, mostly in"
    , " specs. Don't do math on these values."
    ]

instance PersistField (Id a) where
    toPersistValue = toPersistValue . untagId
    fromPersistValue = (mkId Proxy <$>) . fromPersistValue

instance PersistFieldSql (Id a) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

instance PersistField (Name a) where
    toPersistValue = toPersistValue . untagName
    fromPersistValue = (mkName Proxy <$>) . fromPersistValue

instance PersistFieldSql (Name a) where
    sqlType _ = sqlType (Proxy :: Proxy Text)

type InstallationId = Id Installation

type PullRequestId = Id PullRequest

type OwnerName = Name Owner

type RepoName = Name Repo

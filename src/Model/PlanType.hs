{-# LANGUAGE OverloadedStrings #-}

module Model.PlanType
    ( PlanType(..)
    ) where

import ClassyPrelude

import Data.Proxy
import Database.Persist
import Database.Persist.Sql

data PlanType
    = Trial
    deriving (Bounded, Eq, Enum, Show)

instance PersistField PlanType where
    toPersistValue Trial = PersistText "trial"

    fromPersistValue (PersistText "trial") = Right Trial
    fromPersistValue v = Left $ "Unexpected persist type: " <> tshow v

instance PersistFieldSql PlanType where
    sqlType _ = sqlType (Proxy :: Proxy Text)

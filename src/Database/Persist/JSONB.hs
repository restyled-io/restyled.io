module Database.Persist.JSONB
  ( JSONB (..)
  ) where

import Prelude

import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import GHC.Generics
import Test.QuickCheck.Arbitrary

newtype JSONB a = JSONB {unJSONB :: a}
  deriving stock
    ( Eq
    , Foldable
    , Functor
    , Generic
    , Ord
    , Read
    , Show
    , Traversable
    )
  deriving newtype (Arbitrary, FromJSON, ToJSON)

instance (FromJSON a, ToJSON a) => PersistField (JSONB a) where
  toPersistValue = toPersistValueJSON
  fromPersistValue = fromPersistValueJSON

instance (FromJSON a, ToJSON a) => PersistFieldSql (JSONB a) where
  sqlType _ = SqlOther "JSONB"

module Restyled.RestylerImage
  ( RestylerImage
  , restylerImage
  ) where

import Restyled.Prelude

import Database.Persist.Sql (PersistFieldSql)
import Restyled.Prelude.Esqueleto (SqlString)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.Text ()

newtype RestylerImage = RestylerImage Text
  deriving stock (Eq, Show)
  deriving newtype
    ( Arbitrary
    , PersistField
    , PersistFieldSql
    , SqlString
    , FromJSON
    , ToJSON
    )

restylerImage :: Text -> RestylerImage
restylerImage = RestylerImage

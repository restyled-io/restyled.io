module Restyled.RestylerImage
    ( RestylerImage
    , restylerImage
    , unRestylerImage
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

unRestylerImage :: RestylerImage -> Text
unRestylerImage (RestylerImage x) = x

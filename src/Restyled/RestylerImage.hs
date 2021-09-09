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

newtype RestylerImage = RestylerImage
    { unRestylerImage :: Text
    }
    deriving stock (Eq, Show)
    deriving newtype
        ( PersistField
        , PersistFieldSql
        , SqlString
        , FromJSON
        , ToJSON
        )

instance Arbitrary RestylerImage where
    arbitrary = restylerImage <$> arbitrary <*> arbitrary

restylerImage :: Text -> Maybe Text -> RestylerImage
restylerImage image mTag = RestylerImage $ image <> maybe "" (":" <>) mTag

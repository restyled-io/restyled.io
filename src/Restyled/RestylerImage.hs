module Restyled.RestylerImage
    ( RestylerImage
    , restylerImage
    , unRestylerImage
    ) where

import Restyled.Prelude

import Database.Persist.Sql (PersistFieldSql)
import Restyled.Prelude.Esqueleto (SqlString)

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

restylerImage :: Text -> Maybe Text -> RestylerImage
restylerImage image mTag = RestylerImage $ image <> maybe "" (":" <>) mTag

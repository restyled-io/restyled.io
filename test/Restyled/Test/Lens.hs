module Restyled.Test.Lens
    ( fieldLens
    , (.~)
    , (?~)
    , (^.)
    , (^..)
    , (^?)
    ) where

import Restyled.Prelude hiding (fieldLens)

import Control.Lens ((?~))
import qualified Database.Persist as P

fieldLens
    :: PersistEntity record => EntityField record field -> Lens' record field
fieldLens field =
    unsafeEntityLens . P.fieldLens field . unsafeEntityLens . entityValLens

unsafeEntityLens :: Lens' record (Entity record)
unsafeEntityLens = lens (Entity (error err)) (\_ entity -> entityVal entity)
  where
    err :: String
    err = "Cannot access EntityKey of Entity constructed unsafely without one"

-- entityKeyLens :: Lens' (Entity record) (Key record)
-- entityKeyLens = lens entityKey (\(Entity _ v) k -> Entity k v)

entityValLens :: Lens' (Entity record) record
entityValLens = lens entityVal (\(Entity k _) v -> Entity k v)

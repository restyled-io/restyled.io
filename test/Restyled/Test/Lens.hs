module Restyled.Test.Lens
    ( fieldLens
    , (.~)
    , (?~)
    , (^.)
    , (^..)
    , (^?)
    ) where

import Restyled.Prelude hiding (fieldLens)

import qualified Database.Persist as P
import Lens.Micro ((.~), (?~), (^.), (^..), (^?))

-- | Use an 'EntityField' as a lens
--
-- The library-provided @fieldLens@ is over @'Entity' record@, and we need it
-- over just @record@ for Graphula. This is unsafe when used with a key
-- 'EntityField', such as 'JobId', which is why the library-provided one works
-- the way that it does.
--
fieldLens
    :: PersistEntity record => EntityField record field -> Lens' record field
fieldLens field = unsafeEntityLens . P.fieldLens field

unsafeEntityLens :: Lens' record (Entity record)
unsafeEntityLens = lens (Entity (error "unused")) $ \_ y -> entityVal y

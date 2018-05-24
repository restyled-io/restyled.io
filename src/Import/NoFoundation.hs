module Import.NoFoundation
    ( module Import
    , module Import.NoFoundation
    ) where

import ClassyPrelude.Yesod as Import hiding (Proxy)
import Control.Monad.Logger as Import
import Data.Proxy as Import
import Data.Time as Import
import Model as Import
import Model.Repo as Import
import Settings as Import
import Settings.StaticFiles as Import

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM d = maybe d pure

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

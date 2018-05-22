module Import
    ( module Import
    ) where

import Foundation as Import
import Import.NoFoundation as Import

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM d = maybe d pure

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

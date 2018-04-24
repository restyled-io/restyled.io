module Import
    ( module Import
    ) where

import Foundation as Import
import Import.NoFoundation as Import

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM d = maybe d pure

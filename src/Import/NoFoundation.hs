{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( (<$$>)
    , module Import
    ) where

import ClassyPrelude.Yesod   as Import
import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import

--------------------------------------------------------------------------------
-- Basic helpers that should be available everywhere
--------------------------------------------------------------------------------

-- | Double @'fmap'@
--
-- Useful for applying a function to (e.g.) an @'IO' ('Maybe' a)@.
--
(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f = ((f <$>) <$>)

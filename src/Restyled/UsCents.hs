{-# LANGUAGE DerivingVia #-}

module Restyled.UsCents
  ( UsCents
  , fromCents
  , multiplyCents
  ) where

import Restyled.Prelude

import Database.Persist.Sql (PersistFieldSql)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances.Natural ()
import Text.Blaze (ToMarkup (..))

newtype UsCents = UsCents
  { _usCents :: Natural
  }
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary, Num, FromJSON, ToJSON)
  deriving (PersistField, PersistFieldSql) via OverflowNatural

-- TODO: Formatters.fixed 2 + Formatters.commas
instance ToMarkup UsCents where
  toMarkup (UsCents c) =
    mconcat
      ["$", toMarkup $ round @Double @Int $ fromIntegral c / 100, ".00"]

fromCents :: Natural -> UsCents
fromCents = UsCents

multiplyCents :: Integral a => a -> UsCents -> UsCents
multiplyCents a c = fromIntegral a * c

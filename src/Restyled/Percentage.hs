module Restyled.Percentage
  ( Percentage
  , percentage
  ) where

import Restyled.Prelude

import Text.Blaze (ToMarkup (..))

newtype Percentage = Percentage Double

instance ToMarkup Percentage where
  toMarkup (Percentage d) = toMarkup $ show @String (round @_ @Int d) <> "%"

percentage :: (Integral a1, Integral a2) => a1 -> a2 -> Percentage
percentage _ d | d <= 0 = Percentage 0
percentage n d = Percentage $ (fromIntegral n / fromIntegral d) * 100

{-# LANGUAGE DerivingVia #-}

module Restyled.CpuShares
  ( CpuShares (..)
  , cpuSharesToText
  ) where

import Restyled.Prelude

import Database.Persist.Sql (PersistFieldSql)
import Test.QuickCheck

newtype CpuShares = CpuShares
  { unCpuShares :: Natural
  }
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving (PersistField, PersistFieldSql) via OverflowNatural

instance Arbitrary CpuShares where
  arbitrary = CpuShares . fromIntegral @Int . getPositive <$> arbitrary

cpuSharesToText :: CpuShares -> Text
cpuSharesToText = pack . show . unCpuShares

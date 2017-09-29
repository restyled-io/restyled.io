{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Config.Include
    ( Include(..)
    , includePath
    ) where

import ClassyPrelude

import Data.Aeson
import Data.String (IsString(..))
import System.FilePath.Glob (Pattern(), compile, match)

data Include
    = Include Pattern
    | Negated Pattern
    deriving (Eq, Show)

instance FromJSON Include where
    parseJSON = withText "Include pattern" $ pure . fromString . unpack

instance IsString Include where
    fromString ('!':rest) = Negated $ compile rest
    fromString x = Include $ compile x

includePath :: [Include] -> FilePath -> Bool
includePath is fp = foldl' go False is
  where
    go :: Bool -> Include -> Bool
    go b (Include p) = b || p `match` fp
    go b (Negated p) = b && not (p `match` fp)

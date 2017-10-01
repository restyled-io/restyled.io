{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    ) where

import ClassyPrelude

import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

withinClonedRepo :: Text -> IO a -> IO a
withinClonedRepo url act = withSystemTempDirectory "" $ \dir -> do
    callProcess "git" ["clone", unpack url, dir]
    withCurrentDirectory dir act

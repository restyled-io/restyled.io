{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    , checkoutBranch
    , changedPaths
    , commitAll
    , pushOrigin
    ) where

import           ClassyPrelude

import           Model
import           System.Directory (withCurrentDirectory)
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (callProcess, readProcess)

withinClonedRepo :: Text -> IO a -> IO a
withinClonedRepo url act = withSystemTempDirectory "" $ \dir -> do
    callProcess "git" ["clone", unpack url, dir]
    withCurrentDirectory dir act

checkoutBranch :: Bool -> Branch -> IO ()
checkoutBranch b branch = callProcess "git" $
    ["checkout"]
    ++ if b then ["-b"] else []
    ++ [branchArg branch]

changedPaths :: Branch -> IO [FilePath]
changedPaths branch = lines <$>
    readProcess "git" ["diff", "--name-only", branchArg branch] ""

commitAll :: Text -> IO ()
commitAll msg = callProcess "git" ["commit", "-am", unpack msg]

pushOrigin :: Branch -> IO ()
pushOrigin branch = callProcess "git" ["push", "origin", branchArg branch]

branchArg :: Branch -> String
branchArg = unpack . unBranch

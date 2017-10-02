{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.CloneSpec (spec) where

import ClassyPrelude
import Test.Hspec

import GitHub.Model (Branch(..))
import System.Directory (setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)

import Restyler.Clone

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "checkoutBranch" $ do
        it "checks out an existing branch" $ \dir -> do
            setupGitRepo dir
            callProcess "git" ["checkout", "--quiet", "-b", "develop"]

            checkoutBranch False $ Branch "master"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "master\n"

            checkoutBranch False $ Branch "develop"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "develop\n"

        it "checks out a new branch" $ \dir -> do
            setupGitRepo dir

            checkoutBranch True $ Branch "develop"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "develop\n"

setupGitRepo :: FilePath -> IO ()
setupGitRepo dir = do
    setCurrentDirectory dir
    callProcess "git" ["init", "--quiet"]
    callProcess "git" ["commit", "--quiet", "--allow-empty", "--message", "Test"]

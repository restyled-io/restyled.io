{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.CloneSpec (spec) where

import SpecHelper

import Restyler.Clone

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "checkoutBranch" $ do
        it "checks out an existing branch" $ \dir -> do
            setupGitRepo dir
            callProcess "git" ["checkout", "--quiet", "-b", "develop"]

            checkoutBranch False "master"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "master\n"

            checkoutBranch False "develop"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "develop\n"

        it "checks out a new branch" $ \dir -> do
            setupGitRepo dir

            checkoutBranch True "develop"
            readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
                `shouldReturn` "develop\n"

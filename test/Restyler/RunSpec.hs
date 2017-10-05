{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Run
import qualified Data.Text.IO as T

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "callRestylers" $ do
        it "restyles changed files based on config" $ \dir -> do
            setupGitRepo dir

            -- Default config runs stylish-haskell on .hs files
            T.writeFile "Foo.hs" $ dedent [st|
            {-# LANGUAGE OverloadedStrings, RecordWildcards
            #-}
            |]

            callProcess "git" ["add", "Foo.hs"]
            callProcess "git" ["checkout", "--quiet", "-b", "develop"]
            callProcess "git" ["commit", "--quiet", "--message", "Write code"]

            result <- callRestylers "master"
            result `shouldBe` Right ()

            output <- lines <$> readProcess "git" ["diff"] ""
            output `shouldContain`
                [ "-{-# LANGUAGE OverloadedStrings, RecordWildcards"
                , "-#-}"
                ]
            output `shouldContain`
                [ "+{-# LANGUAGE OverloadedStrings #-}"
                , "+{-# LANGUAGE RecordWildcards   #-}"
                ]

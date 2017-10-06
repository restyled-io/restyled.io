{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Run

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    context "Default configuration" $ do
        describe "callRestylers" $ do
            it "restyles Haskell" $ restylerTestCase "Foo.hs"
                [st|
                    {-# LANGUAGE OverloadedStrings, RecordWildcards
                    #-}
                |]
                [ "-{-# LANGUAGE OverloadedStrings, RecordWildcards"
                , "-#-}"
                , "+{-# LANGUAGE OverloadedStrings #-}"
                , "+{-# LANGUAGE RecordWildcards   #-}"
                ]

            it "restyles JavaScript" $ restylerTestCase "foo.js"
                [st|
                    matrix(
                      1, 0, 0,
                      0, 1, 0,
                      0, 0, 1
                    )
                |]
                [ "-matrix("
                , "-  1, 0, 0,"
                , "-  0, 1, 0,"
                , "-  0, 0, 1"
                , "-)"
                , "+matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);"
                ]

restylerTestCase :: FilePath -> Text -> [String] -> FilePath -> Expectation
restylerTestCase name content changes dir = do
    setupGitRepo dir
    setupGitTrackedFile name content $ Just "develop"

    result <- callRestylers "master"
    result `shouldBe` Right ()

    output <- lines <$> readProcess "git" ["diff"] ""
    output `shouldContain` changes

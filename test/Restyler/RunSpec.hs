{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.RunSpec (spec) where

import SpecHelper

import Restyler.Run

spec :: Spec
spec = around (withSystemTempDirectory "") $ do
    describe "callRestylers" $ do
        context "Default configuration" $ do
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

        context "non-default configuration" $ do
            describe "hindent" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["hindent"]
                    setupGitTrackedFile
                        "Foo.hs"
                        "example = case x of Just p -> foo bar\n"
                        $ Just "develop"

                    callRestylers "master" `shouldProduceDiff`
                        [ "-example = case x of Just p -> foo bar"
                        , "+example ="
                        , "+  case x of"
                        , "+    Just p -> foo bar"
                        ]

            describe "brittany" $ do
                it "works" $ \dir -> do
                    setupGitRepo dir
                    setupConfig ["brittany"]
                    setupGitTrackedFile
                        "Foo.hs"
                        (dedent [st|
                            func (MyLongFoo abc def) = 1
                            func (Bar a d) = 2
                            func _ = 3
                        |])
                        $ Just "develop"

                    callRestylers "master" `shouldProduceDiff`
                        [ " func (MyLongFoo abc def) = 1"
                        , "-func (Bar a d) = 2"
                        , "-func _ = 3"
                        , "+func (Bar       a   d  ) = 2"
                        , "+func _                   = 3"
                        ]

restylerTestCase :: FilePath -> Text -> [String] -> FilePath -> Expectation
restylerTestCase name content changes dir = do
    setupGitRepo dir
    setupGitTrackedFile name (dedent content) $ Just "develop"
    callRestylers "master" `shouldProduceDiff` changes

shouldProduceDiff :: IO (Either String ()) -> [String] -> Expectation
shouldProduceDiff call changes = do
    result <- call
    result `shouldBe` Right ()

    output <- lines <$> readProcess "git" ["diff"] ""
    output `shouldContain` changes

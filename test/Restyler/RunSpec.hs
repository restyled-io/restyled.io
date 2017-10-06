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
        it "restyles haskell files" $ \dir -> do
            setupGitRepo dir
            setupCommittedFile "Foo.hs" $ dedent [st|
            {-# LANGUAGE OverloadedStrings, RecordWildcards
            #-}
            |]

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

        it "restyles javascript files" $ \dir -> do
            setupGitRepo dir
            setupCommittedFile "foo.js" $ dedent [st|
            matrix(
              1, 0, 0,
              0, 1, 0,
              0, 0, 1
            )
            |]

            result <- callRestylers "master"
            result `shouldBe` Right ()

            output <- lines <$> readProcess "git" ["diff"] ""
            output `shouldContain`
                [ "-matrix("
                , "-  1, 0, 0,"
                , "-  0, 1, 0,"
                , "-  0, 0, 1"
                , "-)"
                ]
            output `shouldContain`
                [ "+matrix(1, 0, 0, 0, 1, 0, 0, 0, 1);"
                ]

setupCommittedFile :: FilePath -> Text -> IO ()
setupCommittedFile name content = do
    T.writeFile name content
    callProcess "git" ["add", name]
    callProcess "git" ["checkout", "--quiet", "-b", "develop"]
    callProcess "git" ["commit", "--quiet", "--message", "Write code"]

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.ConfigSpec (spec) where

import ClassyPrelude
import Test.Hspec

import Data.Char (isSpace)
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Text.Shakespeare.Text (st)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Prelude as P

import Restyler.Config

spec :: Spec
spec = around withEmptySystemTempFile $ do
    describe "loadConfigFrom" $ do
        it "supports a simple, name-based syntax" $ \tmp -> do
            result <- loadConfig' tmp [st|
            ---
            - stylish-haskell
            - prettier
            |]

            result `shouldBe` Right Config
                { cEnabled = True
                , cRestylers =
                    [ Restyler
                        { rCommand = "stylish-haskell"
                        , rArguments = ["--inplace"]
                        , rInclude = [Include "**/*.hs"]
                        }
                    , Restyler
                        { rCommand = "prettier"
                        , rArguments = ["--write"]
                        , rInclude =
                            [ "**/*.js"
                            , "**/*.jsx"
                            ]
                        }
                    ]
                }

        it "has a setting for globally disabling" $ \tmp -> do
            enabled <- (cEnabled <$>) <$> loadConfig' tmp [st|
            ---
            enabled: false
            restylers:
            - stylish-haskell
            |]

            enabled `shouldBe` Right False

        it "allows re-configuring includes" $ \tmp -> do
            result1 <- loadConfig' tmp [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
            |]

            result2 <- loadConfig' tmp [st|
            ---
            - stylish-haskell:
                include:
                - "**/*.lhs"
            |]

            result3 <- loadConfig' tmp [st|
            ---
            restylers:
            - stylish-haskell:
                include:
                - "**/*.lhs"
            |]

            result1 `shouldBe` result2
            result2 `shouldBe` result3
            result3 `shouldBe` Right Config
                { cEnabled = True
                , cRestylers =
                    [ Restyler
                        { rCommand = "stylish-haskell"
                        , rArguments = ["--inplace"]
                        , rInclude = [Include "**/*.lhs"]
                        }
                    ]
                }

        it "accepts fully-configured restylers" $ \tmp -> do
            result1 <- loadConfig' tmp [st|
            ---
            - command: sh
              arguments:
              - -c
              - tac
              include:
              - "always-reverse-me/*.txt"
              - "!dont-reverse-me/*.txt"
            |]

            result2 <- loadConfig' tmp [st|
            ---
            restylers:
            - command: sh
              arguments:
              - -c
              - tac
              include:
              - "always-reverse-me/*.txt"
              - "!dont-reverse-me/*.txt"
            |]

            result1 `shouldBe` result2
            result2 `shouldBe` Right Config
                { cEnabled = True
                , cRestylers =
                    [ Restyler
                        { rCommand = "sh"
                        , rArguments = ["-c", "tac"]
                        , rInclude =
                            [ Include "always-reverse-me/*.txt"
                            , Negated "dont-reverse-me/*.txt"
                            ]
                        }
                    ]
                }

        it "has good errors for unknown name" $ \tmp -> do
            result1 <- loadConfig' tmp [st|
            ---
            - uknown-name
            |]

            result2 <- loadConfig' tmp [st|
            ---
            - uknown-name:
                arguments:
                - --foo
            |]

            result3 <- loadConfig' tmp [st|
            ---
            restylers:
            - uknown-name:
                arguments:
                - --foo
            |]

            result1 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"
            result2 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"
            result3 `shouldSatisfy` hasError "Unknown restyler name: uknown-name"

loadConfig' :: FilePath -> Text -> IO (Either String Config)
loadConfig' fp yaml = do
    T.writeFile fp $ dedent yaml
    loadConfigFrom fp

hasError :: String -> Either String b -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False

withEmptySystemTempFile :: (FilePath -> IO a) -> IO a
withEmptySystemTempFile = bracket (emptySystemTempFile "") removeFile

dedent :: Text -> Text
dedent t = T.unlines $ map (T.drop $ indent lns) lns
  where
    lns = T.lines $ T.drop 1 t -- assumes an initial "\n"

    indent [] = 0
    indent ts = P.minimum $ map (T.length . T.takeWhile isSpace) ts

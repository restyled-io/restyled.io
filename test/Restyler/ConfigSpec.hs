{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Restyler.ConfigSpec (spec) where

import SpecHelper

import Restyler.Config
import qualified Data.Text.IO as T

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
                        { rName = "stylish-haskell"
                        , rCommand = "stylish-haskell"
                        , rArguments = ["--inplace"]
                        , rInclude = [Include "**/*.hs"]
                        }
                    , Restyler
                        { rName = "prettier"
                        , rCommand = "prettier"
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
                        { rName = "stylish-haskell"
                        , rCommand = "stylish-haskell"
                        , rArguments = ["--inplace"]
                        , rInclude = [Include "**/*.lhs"]
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

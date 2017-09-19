{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.DocsSpec
  ( main
  , spec
  ) where

import TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ describe "Docs" $ it "loads" $ do
    get DocsR
    statusIs 200

{-# LANGUAGE OverloadedStrings #-}
module Handler.DocsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ describe "Docs" $ it "loads" $ do
    get DocsR
    statusIs 200

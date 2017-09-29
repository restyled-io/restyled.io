{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.SignupSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "POST /signups" $ do
        it "inserts the given signup" $ do
            get HomeR -- load signup form

            request $ do
                setUrl SignupR
                setMethod "POST"
                addToken
                byLabel "" "pbrisbin@gmail.com"

            void followRedirect
            htmlAnyContain "#message" "Thank you"

            emails <- map (signupEmail . entityVal) <$> runDB (selectList [] [])
            emails `shouldBe` ["pbrisbin@gmail.com"]

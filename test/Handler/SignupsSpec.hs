{-# LANGUAGE OverloadedStrings #-}
module Handler.SignupsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "POST /signups" $ do
        it "inserts the given signup" $ do
            postForm HomeR SignupR [("", "pbrisbin@gmail.com")]
            void followRedirect

            htmlAnyContain "#message" "Thank you"
            emails <- map (signupEmail . entityVal) <$> runDB (selectList [] [])
            emails `shouldBe` ["pbrisbin@gmail.com"]

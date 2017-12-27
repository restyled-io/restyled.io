{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.WebhooksSpec (spec) where

import TestImport

import Backend.Job
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8

spec :: Spec
spec = withApp $ do
    describe "POST /webhooks" $ do
        it "responds 201 for opened PRs" $ do
            postFixture "webhooks/github/pull-request-opened.json"
            statusIs 201

        it "creates and enqueues a job record" $ do
            postFixture "webhooks/github/pull-request-opened.json"
            statusIs 201

            Just jobD <- runDB $ selectFirst [] []
            Just jobQ <- runBackendTest $ awaitRestylerJob 5

            entityKey jobD `shouldBe` entityKey jobQ
            jobInstallationId (entityVal jobD) `shouldBe` 58920
            jobOwner (entityVal jobD) `shouldBe` "restyled-io"
            jobRepo (entityVal jobD) `shouldBe` "demo"
            jobPullRequest (entityVal jobD) `shouldBe` 1

        -- it "responds 200 for valid but ignored payloads" $ do
        --     need fixtures for some other payloads

        it "responds 4XX for invalid payloads" $ do
            postBody WebhooksR "{}"
            statusIs 400

        it "ignores its own PRs" $ do
            postFixture "webhooks/github/pull-request-opened-restyled.json"
            statusIs 200

        it "ignore private repositories" $ do
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 200

postFixture :: FilePath -> YesodExample App ()
postFixture = postBody WebhooksR <=< readFixture

readFixture :: MonadIO m => FilePath -> m LB.ByteString
readFixture = liftIO . L8.readFile . ("fixtures" </>)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.WebhooksSpec (spec) where

import TestImport

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8

spec :: Spec
spec = withApp $ do
    describe "POST /webhooks" $ do
        it "responds 201 for opened PRs" $ do
            postFixture "webhooks/github/pull-request-opened.json"
            statusIs 201

        -- it "responds 200 for valid but ignored payloads" $ do
        --     need fixtures for some other payloads

        it "responds 4XX for invalid payloads" $ do
            postBody WebhooksR "{}"
            statusIs 400

        it "ignores its own PRs" $ do
            postFixture "webhooks/github/pull-request-opened-restyled.json"
            statusIs 200

postFixture :: FilePath -> YesodExample App ()
postFixture = postBody WebhooksR <=< readFixture

readFixture :: MonadIO m => FilePath -> m LB.ByteString
readFixture = liftIO . L8.readFile . ("fixtures" </>)

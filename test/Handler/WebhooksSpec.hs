{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.WebhooksSpec (spec) where

import TestImport

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Prelude as P

spec :: Spec
spec = withApp $ do
    describe "POST /webhooks" $ do
        it "stores a created payload in the database" $ do
            postFixture "webhooks/github/pull-request-opened.json"
            statusIs 201

            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            webhookPayloadState `shouldBe` Created

        it "stores the payload's Installation Id" $ do
            postFixture "webhooks/github/pull-request-opened.json"

            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            webhookPayloadInstallationId `shouldBe` GitHubId 54123

        it "creates the payload's Repository if not present" $ do
            postFixture "webhooks/github/pull-request-opened.json"

            Entity repositoryId Repository{..} <- expectOne =<< runDB (selectList [] [])
            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            repositoryFullName `shouldBe` "restyled-io/demo"
            webhookPayloadRepository `shouldBe` repositoryId

        it "finds an existing Repository by full name" $ do
            repositoryId <- runDB $ insert Repository
                { repositoryFullName = "restyled-io/demo"
                }

            postFixture "webhooks/github/pull-request-opened.json"

            runDB (count ([] :: [Filter Repository])) `shouldReturn` 1
            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            webhookPayloadRepository `shouldBe` repositoryId

        it "creates the payload's PullRequest if not present" $ do
            postFixture "webhooks/github/pull-request-opened.json"

            Entity pullRequestId PullRequest{..} <- expectOne =<< runDB (selectList [] [])
            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            pullRequestNumber `shouldBe` PRNumber 1
            webhookPayloadPullRequest `shouldBe` pullRequestId

        it "finds an existing PullRequest by repository and number" $ do
            repositoryId <- runDB $ insert Repository
                { repositoryFullName = "restyled-io/demo"
                }
            pullRequestId <- runDB $ insert PullRequest
                { pullRequestNumber = PRNumber 1
                , pullRequestRepository = repositoryId
                }

            postFixture "webhooks/github/pull-request-opened.json"

            runDB (count ([] :: [Filter PullRequest])) `shouldReturn` 1
            Entity _ WebhookPayload{..} <- expectOne =<< runDB (selectList [] [])
            webhookPayloadPullRequest `shouldBe` pullRequestId

postFixture :: FilePath -> YesodExample App ()
postFixture = postBody WebhooksR <=< readFixture

readFixture :: MonadIO m => FilePath -> m LB.ByteString
readFixture = liftIO . L8.readFile . ("fixtures" </>)

expectOne :: Show e => [e] -> YesodExample App e
expectOne xs = do
    xs `shouldSatisfy` ((== 1) . length)
    return $ P.head xs

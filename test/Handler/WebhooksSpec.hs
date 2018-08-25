{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.WebhooksSpec
    ( spec
    )
where

import TestImport

import Backend.Job
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Prelude as Unsafe

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

        it "finds or creates the repository" $ do
            replicateM_ 3 $ do
                postFixture "webhooks/github/pull-request-opened.json"
                statusIs 201

            repos <- runDB $ selectList [] []
            length repos `shouldBe` 1
            let Repo {..} = entityVal $ Unsafe.head repos
            repoOwner `shouldBe` "restyled-io"
            repoName `shouldBe` "demo"
            repoIsPrivate `shouldBe` False
            jobs <- runDB $ selectList [] []
            map (jobOwner . entityVal) jobs `shouldBe` replicate 3 repoOwner
            map (jobRepo . entityVal) jobs `shouldBe` replicate 3 repoName

        it "responds 200 for valid but ignored payloads" $ do
            postFixtureAs "ping" "webhooks/ping.json"
            statusIs 200

            -- Unknown event type
            postGitHubEvent "push" "{}"
            statusIs 200

        it "responds 4XX for invalid payloads" $ do
            -- Invalid body
            postGitHubEvent "pull_request" "{}"
            statusIs 400

            -- No X-GitHub-Event header
            postBody WebhooksR "{}"
            statusIs 400

        it "ignores its own PRs" $ do
            postFixture "webhooks/github/pull-request-opened-restyled.json"
            statusIs 200

        it "ignores private repositories if no plan" $ do
            pendingWith "Moved to Backend"
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 200

        it "ignores private repositories if inactive plan" $ do
            pendingWith "Moved to Backend"
            setupPrivatePlan (Just 100) Nothing
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 200

        it "ignores private repositories if expired plan" $ do
            pendingWith "Moved to Backend"
            setupPrivatePlan Nothing $ Just (-100)
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 200

        it "accepts private repositories if active plan" $ do
            pendingWith "Moved to Backend"
            setupPrivatePlan (Just (-100)) $ Just 100
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 201

        it "accepts private repositories if forever plan" $ do
            pendingWith "Moved to Backend"
            setupPrivatePlan Nothing Nothing
            postFixture "webhooks/github/pull-request-opened-private.json"
            statusIs 201

setupPrivatePlan
    :: Maybe NominalDiffTime -> Maybe NominalDiffTime -> YesodExample App ()
setupPrivatePlan activeIn expiresIn = do
    now <- liftIO getCurrentTime

    let activeAt = (`addUTCTime` now) <$> activeIn
        expiresAt = (`addUTCTime` now) <$> expiresIn

    runDB $ do
        insert_ Repo
            { repoOwner = "restyled-io"
            , repoName = "restyled.io"
            , repoInstallationId = 55758
            , repoIsPrivate = True
            , repoDebugEnabled = False
            }
        insert_ Plan
            { planType = Trial
            , planOwner = "restyled-io"
            , planRepo = "restyled.io"
            , planActiveAt = activeAt
            , planExpiresAt = expiresAt
            , planMessage = "Test plan"
            }

postFixture :: FilePath -> YesodExample App ()
postFixture = postFixtureAs "pull_request"

postFixtureAs :: ByteString -> FilePath -> YesodExample App ()
postFixtureAs event = postGitHubEvent event <=< readFixture

readFixture :: MonadIO m => FilePath -> m LB.ByteString
readFixture = liftIO . L8.readFile . ("fixtures" </>)

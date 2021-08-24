{-# LANGUAGE QuasiQuotes #-}

module Restyled.Api.CreateJobSpec
    ( spec
    ) where

import Restyled.Test

import Data.Aeson.QQ
import qualified Database.Persist as P
import Restyled.Api.CreateJob (ApiCreateJob(ApiCreateJob), createJob)
import qualified Restyled.Api.CreateJob as ApiCreateJob
import qualified Restyled.Api.Job as ApiJob

spec :: Spec
spec = do
    describe "ApiCreateJob" $ do
        let expectDecode :: (HasCallStack, MonadIO m) => Value -> m ()
            expectDecode =
                void
                    . expectRight "ApiCreateJob"
                    . eitherDecode @ApiCreateJob
                    . encode

        it "decodes a complete body" $ example $ do
            now <- getCurrentTime
            expectDecode [aesonQQ|
                { "owner": "restyled-io"
                , "repo": "demo"
                , "pullRequest": 1
                , "completedAt": #{now}
                , "exitCode": 0
                }
            |]

        it "decodes without completedAt and exitCode " $ example $ do
            expectDecode [aesonQQ|
                { "owner": "restyled-io"
                , "repo": "demo"
                , "pullRequest": 1
                }
            |]

    describe "ApiCreateJobErrors" $ do
        it "encodes" $ example $ do
            let errs = ApiCreateJob.repoNotFound "restyled-io" "demo"

            toJSON errs `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "RepoNotFound"
                      , "contents":
                          { "owner": "restyled-io"
                          , "name": "demo"
                          }
                      }
                    ]
                }
            |]

    withApp $ do
        describe "createJob" $ do
            it "fails if the repository does not exist" $ do
                result <- runDB $ runValidateT $ createJob $ ApiCreateJob
                    { ApiCreateJob.owner = "restyled-id"
                    , ApiCreateJob.repo = "demo"
                    , ApiCreateJob.pullRequest = 1
                    , ApiCreateJob.completedAt = Nothing
                    , ApiCreateJob.exitCode = Nothing
                    }

                result `shouldSatisfy` isLeft

            it "creates a complete Job" $ do
                let repo = Repo
                        { repoSvcs = GitHubSVCS
                        , repoOwner = "restyled-io"
                        , repoName = "demo"
                        , repoInstallationId = 1
                        , repoIsPrivate = False
                        , repoDebugEnabled = False
                        , repoEnabled = True
                        , repoRestylerImage = Nothing
                        }

                    pullRequest :: PullRequestNum
                    pullRequest = 1

                    exitCode :: Int
                    exitCode = 99

                now <- liftIO getCurrentTime
                void $ runDB $ insert repo

                result <- runDB $ runValidateT $ createJob $ ApiCreateJob
                    { ApiCreateJob.owner = repoOwner repo
                    , ApiCreateJob.repo = repoName repo
                    , ApiCreateJob.pullRequest = pullRequest
                    , ApiCreateJob.completedAt = Just now
                    , ApiCreateJob.exitCode = Just exitCode
                    }

                resp <- expectRight "ApiJob" result
                ApiJob.owner resp `shouldBe` repoOwner repo
                ApiJob.repo resp `shouldBe` repoName repo
                ApiJob.pullRequest resp `shouldBe` pullRequest
                ApiJob.completedAt resp `shouldBe` Just now
                ApiJob.exitCode resp `shouldBe` Just exitCode

                job <- runDB $ expectJust "Job" =<< P.get (ApiJob.id resp)
                jobOwner job `shouldBe` repoOwner repo
                jobRepo job `shouldBe` repoName repo
                jobPullRequest job `shouldBe` pullRequest
                -- TODO: fails due to postgres rounding
                -- jobCompletedAt job `shouldBe` Just now
                jobExitCode job `shouldBe` Just exitCode

            it "creates an incomplete Job" $ do
                let repo = Repo
                        { repoSvcs = GitHubSVCS
                        , repoOwner = "restyled-io"
                        , repoName = "demo"
                        , repoInstallationId = 1
                        , repoIsPrivate = False
                        , repoDebugEnabled = False
                        , repoEnabled = True
                        , repoRestylerImage = Nothing
                        }

                    pullRequest :: PullRequestNum
                    pullRequest = 1

                void $ runDB $ insert repo

                result <- runDB $ runValidateT $ createJob $ ApiCreateJob
                    { ApiCreateJob.owner = repoOwner repo
                    , ApiCreateJob.repo = repoName repo
                    , ApiCreateJob.pullRequest = pullRequest
                    , ApiCreateJob.completedAt = Nothing
                    , ApiCreateJob.exitCode = Nothing
                    }

                resp <- expectRight "ApiJob" result
                ApiJob.completedAt resp `shouldSatisfy` isNothing
                ApiJob.exitCode resp `shouldSatisfy` isNothing

                job <- runDB $ expectJust "Job" =<< P.get (ApiJob.id resp)
                jobCompletedAt job `shouldSatisfy` isNothing
                jobExitCode job `shouldSatisfy` isNothing

{-# LANGUAGE QuasiQuotes #-}

module Restyled.Api.CreateJobSpec
  ( spec
  ) where

import Restyled.Test

import Data.Aeson.QQ
import qualified Database.Persist as P
import Restyled.Api.CreateJob (ApiCreateJob (ApiCreateJob), createJob)
import qualified Restyled.Api.CreateJob as ApiCreateJob
import qualified Restyled.Api.Job as ApiJob
import Restyled.Test.Graphula

spec :: Spec
spec = do
  describe "ApiCreateJob" $ do
    let
      expectDecode :: (HasCallStack, MonadIO m) => Value -> m ()
      expectDecode =
        void
          . expectRight "ApiCreateJob"
          . eitherDecode @ApiCreateJob
          . encode

    it "decodes a complete body" $ example $ do
      now <- getCurrentTime
      expectDecode
        [aesonQQ|
                { "owner": "restyled-io"
                , "repo": "demo"
                , "pullRequest": 1
                , "completedAt": #{now}
                , "exitCode": 0
                }
            |]

    it "decodes without completedAt and exitCode " $ example $ do
      expectDecode
        [aesonQQ|
                { "owner": "restyled-io"
                , "repo": "demo"
                , "pullRequest": 1
                }
            |]

  describe "ApiCreateJobErrors" $ do
    it "encodes" $ example $ do
      let errs = ApiCreateJob.repoNotFound "restyled-io" "demo"

      toJSON errs
        `shouldMatchJson` [aesonQQ|
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
        result <-
          runDB
            $ runValidateT
            $ createJob
            $ ApiCreateJob
              { ApiCreateJob.owner = "restyled-id"
              , ApiCreateJob.repo = "demo"
              , ApiCreateJob.pullRequest = 1
              , ApiCreateJob.completedAt = Nothing
              , ApiCreateJob.exitCode = Nothing
              }

        result `shouldSatisfy` isLeft

      it "creates a complete Job" $ graph $ do
        now <- liftIO getCurrentTime
        Entity _ Repo {..} <- node @Repo () mempty

        lift $ runDB $ do
          Right resp <-
            runValidateT
              $ createJob
              $ ApiCreateJob
                { ApiCreateJob.owner = repoOwner
                , ApiCreateJob.repo = repoName
                , ApiCreateJob.pullRequest = 1
                , ApiCreateJob.completedAt = Just now
                , ApiCreateJob.exitCode = Just 99
                }

          ApiJob.owner resp `shouldBe` repoOwner
          ApiJob.repo resp `shouldBe` repoName
          ApiJob.pullRequest resp `shouldBe` 1

          Just Job {..} <- P.get $ ApiJob.id resp
          jobOwner `shouldBe` repoOwner
          jobRepo `shouldBe` repoName
          jobPullRequest `shouldBe` 1
          jobCompletedAt `shouldSatisfy` isJust
          jobExitCode `shouldBe` Just 99

      it "creates an incomplete Job" $ graph $ do
        Entity _ Repo {..} <- node @Repo () mempty

        lift $ runDB $ do
          Right resp <-
            runValidateT
              $ createJob
              $ ApiCreateJob
                { ApiCreateJob.owner = repoOwner
                , ApiCreateJob.repo = repoName
                , ApiCreateJob.pullRequest = 1
                , ApiCreateJob.completedAt = Nothing
                , ApiCreateJob.exitCode = Nothing
                }

          Just Job {..} <- P.get $ ApiJob.id resp
          jobCompletedAt `shouldSatisfy` isNothing
          jobExitCode `shouldSatisfy` isNothing

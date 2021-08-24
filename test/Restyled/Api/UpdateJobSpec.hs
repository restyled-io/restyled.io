{-# LANGUAGE QuasiQuotes #-}

module Restyled.Api.UpdateJobSpec
    ( spec
    ) where

import Restyled.Test

import Data.Aeson.QQ
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)
import qualified Restyled.Api.Job as ApiJob
import Restyled.Api.UpdateJob
    (ApiCompleteJob(ApiCompleteJob), ApiUpdateJob(Complete), updateJob)
import qualified Restyled.Api.UpdateJob as ApiUpdateJob

spec :: Spec
spec = do
    describe "ApiUpdateJob" $ do
        let expectDecode :: (HasCallStack, MonadIO m) => Value -> m ()
            expectDecode =
                void
                    . expectRight "ApiUpdateJob"
                    . eitherDecode @ApiUpdateJob
                    . encode

        it "decodes" $ example $ do
            now <- getCurrentTime
            expectDecode [aesonQQ|
                { "tag": "Complete"
                , "contents":
                    { "completedAt": #{now}
                    , "exitCode": 0
                    }
                }
            |]

    describe "ApiUpdateJobErrors" $ do
        it "encodes" $ example $ do
            let errs = ApiUpdateJob.updatedJobNotFound $ toSqlKey 99

            toJSON errs `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "UpdatedJobNotFound"
                      , "contents": 99
                      }
                    ]
                }
            |]

    withApp $ do
        describe "updateJob" $ do
            it "sets values given" $ runDB $ do
                now <- liftIO getCurrentTime
                jobId <- insert $ Job
                    { jobSvcs = GitHubSVCS
                    , jobOwner = "foo"
                    , jobRepo = "bar"
                    , jobPullRequest = 1
                    , jobCreatedAt = now
                    , jobUpdatedAt = now
                    , jobCompletedAt = Nothing
                    , jobExitCode = Nothing

                    -- Legacy
                    , jobLog = Nothing
                    , jobStdout = Nothing
                    , jobStderr = Nothing
                    }

                void
                    $ assertValidateT
                    $ updateJob jobId
                    $ pure
                    $ Complete
                    $ ApiCompleteJob
                          { ApiUpdateJob.completedAt = now
                          , ApiUpdateJob.exitCode = 99
                          }

                Just job <- P.get jobId

                jobCompletedAt job `shouldSatisfy` isJust
                jobExitCode job `shouldBe` Just 99

            it "returns an updated ApiJob" $ runDB $ do
                now <- liftIO getCurrentTime
                jobId <- insert $ Job
                    { jobSvcs = GitHubSVCS
                    , jobOwner = "foo"
                    , jobRepo = "bar"
                    , jobPullRequest = 1
                    , jobCreatedAt = now
                    , jobUpdatedAt = now
                    , jobCompletedAt = Nothing
                    , jobExitCode = Nothing

                    -- Legacy
                    , jobLog = Nothing
                    , jobStdout = Nothing
                    , jobStderr = Nothing
                    }

                Right job <-
                    runValidateT
                    $ updateJob jobId
                    $ pure
                    $ Complete
                    $ ApiCompleteJob
                          { ApiUpdateJob.completedAt = now
                          , ApiUpdateJob.exitCode = 99
                          }

                ApiJob.completedAt job `shouldSatisfy` isJust
                ApiJob.exitCode job `shouldBe` Just 99

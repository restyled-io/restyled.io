{-# LANGUAGE QuasiQuotes #-}

module Restyled.Handlers.JobsSpec
    ( spec
    ) where

import Restyled.Test

import Control.Lens ((^?!))
import Data.Aeson.Lens
import Data.Aeson.QQ
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)

spec :: Spec
spec = withApp $ do
    describe "PATCH /jobs/:id" $ do
        it "404s for unknown Job" $ do
            void authenticateAsAdmin

            patchJSON (JobsP $ JobP (toSqlKey 99) JobR) [aesonQQ|{}|]

            statusIs 404

        it "expects a nonempty list of updates" $ do
            void authenticateAsAdmin
            now <- liftIO getCurrentTime
            jobId <- runDB $ do
                let
                    repo = Repo
                        { repoSvcs = GitHubSVCS
                        , repoOwner = "foo"
                        , repoName = "bar"
                        , repoInstallationId = 1
                        , repoIsPrivate = False
                        , repoDebugEnabled = False
                        , repoEnabled = True
                        , repoRestylerImage = Nothing
                        }
                insert_ repo
                insert Job
                    { jobSvcs = repoSvcs repo
                    , jobOwner = repoOwner repo
                    , jobRepo = repoName repo
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

            patchJSON (JobsP $ JobP jobId JobR) [aesonQQ|[]|]

            statusIs 400
            resp <- getJsonBody
            resp
                ^?! key "errors"
                . nth 0
                . _String
                `shouldSatisfy` ("parsing NonEmpty failed" `T.isPrefixOf`)

        it "accepts updates and parrots back" $ do
            void authenticateAsAdmin
            now <- liftIO getCurrentTime
            jobId <- runDB $ do
                let
                    repo = Repo
                        { repoSvcs = GitHubSVCS
                        , repoOwner = "foo"
                        , repoName = "bar"
                        , repoInstallationId = 1
                        , repoIsPrivate = False
                        , repoDebugEnabled = False
                        , repoEnabled = True
                        , repoRestylerImage = Nothing
                        }
                insert_ repo
                insert Job
                    { jobSvcs = repoSvcs repo
                    , jobOwner = repoOwner repo
                    , jobRepo = repoName repo
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

            patchJSON
                (JobsP $ JobP jobId JobR)
                [aesonQQ|
                [ { "tag": "Complete"
                  , "contents":
                    { "completedAt": #{now}
                    , "exitCode": 99
                    }
                  }
                ]
            |]

            statusIs 200
            resp <- getJsonBody
            resp ^?! key "exitCode" . _Number `shouldBe` 99

            Just Job {..} <- runDB $ P.get jobId
            jobExitCode `shouldBe` Just 99

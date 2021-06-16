module Restyled.MetricsSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Metrics
import Restyled.Time
import Restyled.TimeRange

spec :: Spec
spec = withApp $ do
    describe "fetchJobMetrics" $ do
        it "returns counts of Jobs in various states" $ do
            JobMetrics {..} <- runDB $ do
                now <- liftIO getCurrentTime
                range <- timeRangeFromAgo $ Minutes 5
                repo <- insertEntity $ buildRepo "pbrisbin" "test"
                insertMany_
                    [ buildCompleteJob repo now now 0
                    , buildIncompleteJob repo now
                    , buildCompleteJob repo now now 20
                    , buildCompleteJob repo now now 0
                    , buildCompleteJob repo now now 99
                    , buildIncompleteJob repo now
                    , buildIncompleteJob repo now
                    , buildCompleteJob repo now now 0
                    , buildIncompleteJob repo now
                    , buildIncompleteJob repo now
                    ]

                fetchJobMetrics range

            jmSucceeded `shouldBe` Sum 3
            jmFailed `shouldBe` Sum 2
            jmFailedUnknown `shouldBe` Sum 1
            jmUnfinished `shouldBe` Sum 5
            jmTotal `shouldBe` 10

buildIncompleteJob :: Entity Repo -> UTCTime -> Job
buildIncompleteJob repo createdAt = buildJob repo createdAt Nothing Nothing

buildCompleteJob :: Entity Repo -> UTCTime -> UTCTime -> Int -> Job
buildCompleteJob repo createdAt completedAt exitCode =
    buildJob repo createdAt (Just completedAt) (Just exitCode)

buildJob :: Entity Repo -> UTCTime -> Maybe UTCTime -> Maybe Int -> Job
buildJob (Entity _ Repo {..}) createdAt mCompletedAt mExitCode = Job
    { jobSvcs = GitHubSVCS
    , jobOwner = repoOwner
    , jobRepo = repoName
    , jobPullRequest = 1
    , jobCreatedAt = createdAt
    , jobUpdatedAt = fromMaybe createdAt mCompletedAt
    , jobCompletedAt = mCompletedAt
    , jobExitCode = mExitCode
    , jobLog = Nothing
    , jobStdout = Nothing
    , jobStderr = Nothing
    }

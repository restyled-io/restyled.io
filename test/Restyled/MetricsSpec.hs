module Restyled.MetricsSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Metrics
import Restyled.Test.Graphula
import Restyled.Time
import Restyled.TimeRange

spec :: Spec
spec = withApp $ do
    describe "fetchJobMetrics" $ do
        it "returns counts of Jobs in various states" $ graph $ do
            now <- liftIO getCurrentTime
            repo <- node @Repo () mempty
            let setJobCreatedAt = fieldLens JobCreatedAt .~ now
            sequence_
                [ genJob repo $ setJobCreatedAt . setJobComplete now 0
                , genJob repo $ setJobCreatedAt . setJobIncomplete
                , genJob repo $ setJobCreatedAt . setJobComplete now 20
                , genJob repo $ setJobCreatedAt . setJobComplete now 0
                , genJob repo $ setJobCreatedAt . setJobComplete now 99
                , genJob repo $ setJobCreatedAt . setJobIncomplete
                , genJob repo $ setJobCreatedAt . setJobIncomplete
                , genJob repo $ setJobCreatedAt . setJobComplete now 0
                , genJob repo $ setJobCreatedAt . setJobIncomplete
                , genJob repo $ setJobCreatedAt . setJobIncomplete
                ]

            range <- timeRangeFromAgo $ Minutes 5
            JobMetrics {..} <- lift $ runDB $ fetchJobMetrics range

            jmSucceeded `shouldBe` Sum 3
            jmFailed `shouldBe` Sum 2
            jmFailedUnknown `shouldBe` Sum 1
            jmUnfinished `shouldBe` Sum 5
            jmTotal `shouldBe` 10

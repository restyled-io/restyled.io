module Restyled.Handlers.Admin.MetricsSpec
    ( spec
    )
where

import Restyled.Test

import Data.Aeson.Lens
import qualified Prelude as Unsafe

spec :: Spec
spec = withApp $ do
    describe "GET /admin/metrics" $ do
        it "requires TimeRange parameters" $ do
            void authenticateAsAdmin

            get $ AdminP AdminMetricsR
            statusIs 400

            getWith [("from", "1562100572")] $ AdminP AdminMetricsR
            statusIs 400

            getWith [("to", "1562100572")] $ AdminP AdminMetricsR
            statusIs 400

            getWith [("from", "1562100572"), ("to", "1562100572")]
                $ AdminP AdminMetricsR
            statusIs 200

        it "returns jobs in the given range" $ do
            let t1 = makeTime 2019 1 1
                t2 = makeTime 2019 1 2
                t3 = makeTime 2019 1 3
                t4 = makeTime 2019 1 4
                t5 = makeTime 2019 1 5
            runDB $ do
                _beforeRange <- insert $ jobAt t1 $ Just (t1, 0)
                _successInRange1 <- insert $ jobAt t1 $ Just (t2, 0)
                _failureInRange2 <- insert $ jobAt t1 $ Just (t2, 1)
                _unfinishedInRange <- insert $ jobAt t3 Nothing
                _beyondRange <- insert $ jobAt t5 $ Just (t5, 0)
                pure ()

            void authenticateAsAdmin
            getWith (toTimeRangeParams t2 t4) $ AdminP AdminMetricsR

            statusIs 200
            body <- getBody
            body
                ^.. _Array
                . traverse
                . _JSON
                `shouldMatchList` [ object
                                      [ "Date" .= toEpoch t2
                                      , "Succeeded" .= (1 :: Int)
                                      , "Failed" .= (1 :: Int)
                                      , "Unfinished" .= (0 :: Int)
                                      ]
                                  , object
                                      [ "Date" .= toEpoch t3
                                      , "Succeeded" .= (0 :: Int)
                                      , "Failed" .= (0 :: Int)
                                      , "Unfinished" .= (1 :: Int)
                                      ]
                                  ]

makeTime :: Integer -> Int -> Int -> UTCTime
makeTime y m d = UTCTime (fromGregorian y m d) 0

jobAt :: UTCTime -> Maybe (UTCTime, Int) -> Job
jobAt createdAt mCompletion = Job
    { jobSvcs = GitHubSVCS
    , jobOwner = "some-owner"
    , jobRepo = "some-repo"
    , jobPullRequest = 1
    , jobCreatedAt = createdAt
    , jobUpdatedAt = fromMaybe createdAt $ fst <$> mCompletion
    , jobCompletedAt = fst <$> mCompletion
    , jobExitCode = snd <$> mCompletion
    , jobLog = Nothing
    , jobStdout = Nothing
    , jobStderr = Nothing
    }

toTimeRangeParams :: UTCTime -> UTCTime -> [(Text, Text)]
toTimeRangeParams t1 t2 = [("from", toEpochParam t1), ("to", toEpochParam t2)]

toEpoch :: UTCTime -> Int
toEpoch = Unsafe.read . formatTime defaultTimeLocale "%s"

toEpochParam :: UTCTime -> Text
toEpochParam = pack . formatTime defaultTimeLocale "%s"

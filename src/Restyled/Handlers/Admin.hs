{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin
    ( getAdminR
    , getAdminStatsReposR
    , getAdminStatsJobsR
    )
where

import Restyled.Prelude.Esqueleto

import Control.Lens (_1, _3)
import Data.Semigroup (getSum)
import Formatting (format)
import Formatting.Formatters as Formatters
import Restyled.Foundation
import Restyled.Metrics
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.TimeRange
import Restyled.Yesod
import Text.Blaze (ToMarkup(..))

newtype Percentage = Percentage Double

instance ToMarkup Percentage where
    toMarkup (Percentage d) = toMarkup $ show (round @_ @Int d) <> "%"

percent :: Int -> Int -> Percentage
percent _ d | d <= 0 = Percentage 0
percent n d = Percentage $ (fromIntegral n / fromIntegral d) * 100

getAdminR :: Handler Html
getAdminR = adminLayout $ do
    setTitle "Restyled Admin"
    $(widgetFile "admin/dashboard")

data RepoStats = RepoStats
    { totalRepos :: Int
    , activeRepos :: Int
    , activeReposPercent :: Percentage
    , uniqueOwners :: Int
    }

getAdminStatsReposR :: Handler Html
getAdminStatsReposR = do
    (range, timeRange) <- getAdminStatsTimeRange
    RepoStats {..} <- runDB $ fetchRepoStats timeRange

    fragmentLayout [whamlet|
        <p>#{pluralizeWith Formatters.commas "Repo" "Repos" totalRepos}
        <p>#{pluralizeWith Formatters.commas "Unique Owner" "Unique Owners" uniqueOwners}
        <p>#{format Formatters.commas activeRepos} (#{activeReposPercent}) with Jobs this #{show range}
    |]

-- brittany-disable-next-binding

fetchRepoStats :: MonadIO m => TimeRange -> SqlPersistT m RepoStats
fetchRepoStats timeRange = do
    ownerNamesJobs <-
        -- N.B. we're using ^. on the jobs even though it's an OUTER JOIN and so
        -- it could be nullable. It's safe because we don't return any possibly-
        -- Nothing fields as non-Maybe values to Haskell. And it makes it much
        -- easier to work with (e.g. for withinTimeRange)
        selectMap unValue3 . from $ \(repos `LeftOuterJoin` jobs) -> do
            on $ repos ^. RepoOwner ==. jobs ^. JobOwner
                &&. repos ^. RepoName ==. jobs ^. JobRepo
                &&. jobs ^. JobCreatedAt `withinTimeRange` timeRange
            groupBy (repos ^. RepoOwner, repos ^. RepoName)
            pure
                ( repos ^. RepoOwner
                , repos ^. RepoName
                , count @Int $ jobs ^. persistIdField
                )

    let totalRepos = length ownerNamesJobs
        activeRepos = length $ filter ((> 0) . view _3) ownerNamesJobs
        activeReposPercent = percent activeRepos totalRepos
        uniqueOwners = length $ nubOrd $ map (view _1) ownerNamesJobs
    pure RepoStats { .. }

data JobStats = JobStats
    { totalJobs :: Int
    , succeededJobs :: Int
    , succeededJobsPercent :: Percentage
    , failedJobs :: Int
    , failedJobsPercent :: Percentage
    , incompleteJobs :: Int
    , incompleteJobsPercent :: Percentage
    }

getAdminStatsJobsR :: Handler Html
getAdminStatsJobsR = do
    (range, timeRange) <- getAdminStatsTimeRange
    JobStats {..} <- runDB $ fetchJobStats timeRange

    fragmentLayout [whamlet|
        <p>#{pluralizeWith Formatters.commas "job" "jobs" totalJobs} Jobs this #{show range}
        <p>#{format Formatters.commas succeededJobs} (#{succeededJobsPercent}) succeeded
        <p>#{format Formatters.commas failedJobs} (#{failedJobsPercent}) failed
        <p>#{format Formatters.commas incompleteJobs} (#{incompleteJobsPercent}) incomplete
    |]

fetchJobStats :: MonadIO m => TimeRange -> SqlPersistT m JobStats
fetchJobStats timeRange = do
    JobMetrics {..} <- fetchJobMetrics timeRange

    let totalJobs = getSum $ jmSucceeded + jmFailed + jmUnfinished
        succeededJobs = getSum jmSucceeded
        succeededJobsPercent = percent succeededJobs totalJobs
        failedJobs = getSum jmFailed
        failedJobsPercent = percent failedJobs totalJobs
        incompleteJobs = getSum jmUnfinished
        incompleteJobsPercent = percent incompleteJobs totalJobs

    pure $ JobStats { .. }

data Range
    = Week
    | Month
    | Year
    deriving stock (Eq, Show, Bounded, Enum)

rangeOptions :: [(Text, Range)]
rangeOptions = [("week", Week), ("month", Month), ("year", Year)]

getAdminStatsTimeRange :: Handler (Range, TimeRange)
getAdminStatsTimeRange = do
    range <- fmap (fromMaybe Week) $ runInputGet $ iopt
        (selectFieldList rangeOptions)
        "range"

    timeRange <- timeRangeFromMinutesAgo $ case range of
        Week -> 60 * 24 * 7
        Month -> 60 * 24 * 31
        Year -> 60 * 24 * 365

    pure (range, timeRange)

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
import Data.List (elemIndex)
import Data.Semigroup (getSum)
import Formatting (format)
import Formatting.Formatters as Formatters
import Restyled.Foundation
import Restyled.Metrics
import Restyled.Models
import Restyled.Percentage
import Restyled.Routes
import Restyled.Settings
import Restyled.TimeRange
import Restyled.Yesod
import Text.Blaze (ToMarkup(..))

getAdminR :: Handler Html
getAdminR = adminLayout $ do
    setTitle "Restyled Admin"
    $(widgetFile "admin/dashboard")

data Changed n
    = Increased n
    | Decreased n

changedPercentage :: Int -> Int -> Changed Percentage
changedPercentage before after
    | after >= before = Increased $ percentage (after - before) before
    | otherwise = Decreased $ percentage (before - after) before

changedWidget :: ToMarkup n => Changed n -> Widget
changedWidget = \case
    Increased n -> [whamlet|<span .increased>#{n}|]
    Decreased n -> [whamlet|<span .decreased>#{n}|]

data RepoStats = RepoStats
    { totalRepos :: Int
    , activeRepos :: Int
    , activeReposPercent :: Percentage
    , activeReposChanged :: Changed Percentage
    , uniqueOwners :: Int
    , uniqueOwnersChanged :: Changed Percentage
    }

getAdminStatsReposR :: Handler Html
getAdminStatsReposR = do
    (range, timeRange) <- getAdminStatsTimeRange
    RepoStats {..} <- runDB $ fetchRepoStats timeRange

    fragmentLayout [whamlet|
        <p>#{pluralizeWith Formatters.commas "Repo" "Repos" totalRepos}
        <p>
            #{pluralizeWith Formatters.commas "Unique Owner" "Unique Owners" uniqueOwners}
            ^{changedWidget uniqueOwnersChanged}
        <p>
            #{format Formatters.commas activeRepos} (#{activeReposPercent}) with Jobs this #{show range}
            ^{changedWidget activeReposChanged}
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

    ownerNamesPrevious <-
        selectMap unValue2 . from $ \(repos `InnerJoin` jobs) -> do
            on $ repos ^. RepoOwner ==. jobs ^. JobOwner
                &&. repos ^. RepoName ==. jobs ^. JobRepo
                &&. jobs ^. JobCreatedAt `withinTimeRange` timeRangeBefore timeRange
            groupBy (repos ^. RepoOwner, repos ^. RepoName)
            pure
                ( repos ^. RepoOwner
                , repos ^. RepoName
                )

    let totalRepos = length ownerNamesJobs
        activeRepos = length $ filter ((> 0) . view _3) ownerNamesJobs
        activeReposPercent = percentage activeRepos totalRepos
        activeReposChanged =
            changedPercentage (length ownerNamesPrevious) activeRepos
        uniqueOwners = length $ nubOrd $ map (view _1) ownerNamesJobs
        uniqueOwnersChanged = changedPercentage
            (length $ nubOrd $ map (view _1) ownerNamesPrevious)
            uniqueOwners
    pure RepoStats { .. }

data JobStats = JobStats
    { totalJobs :: Int
    , totalJobsChanged :: Changed Percentage
    , succeededJobs :: Int
    , succeededJobsPercent :: Percentage
    , succeededJobsChanged :: Changed Percentage
    , failedJobs :: Int
    , failedJobsPercent :: Percentage
    , failedJobsChanged :: Changed Percentage
    , unfinishedJobs :: Int
    , unfinishedJobsPercent :: Percentage
    , unfinishedJobsChanged :: Changed Percentage
    }

getAdminStatsJobsR :: Handler Html
getAdminStatsJobsR = do
    (range, timeRange) <- getAdminStatsTimeRange
    JobStats {..} <- runDB $ fetchJobStats timeRange

    fragmentLayout [whamlet|
        <p>
            #{pluralizeWith Formatters.commas "Job" "Jobs" totalJobs} this #{show range}
            ^{changedWidget totalJobsChanged}
        <p>
            #{format Formatters.commas succeededJobs} (#{succeededJobsPercent}) succeeded
            ^{changedWidget succeededJobsChanged}
        <p>
            #{format Formatters.commas failedJobs} (#{failedJobsPercent}) failed
            ^{changedWidget failedJobsChanged}
        <p>
            #{format Formatters.commas unfinishedJobs} (#{unfinishedJobsPercent}) unfinished
            ^{changedWidget unfinishedJobsChanged}
    |]

fetchJobStats :: MonadIO m => TimeRange -> SqlPersistT m JobStats
fetchJobStats timeRange = do
    metrics <- fetchJobMetrics timeRange
    metricsPrevious <- fetchJobMetrics $ timeRangeBefore timeRange

    let totalJobs = getSum $ jmTotal metrics
        totalJobsChanged = changedPercentage
            (getSum $ jmTotal metrics)
            (getSum $ jmTotal metricsPrevious)
        succeededJobs = getSum $ jmSucceeded metrics
        succeededJobsPercent = percentage succeededJobs totalJobs
        succeededJobsChanged = changedPercentage
            (getSum $ jmSucceeded metrics)
            (getSum $ jmSucceeded metricsPrevious)
        failedJobs = getSum $ jmFailed metrics
        failedJobsPercent = percentage failedJobs totalJobs
        failedJobsChanged = changedPercentage
            (getSum $ jmFailed metrics)
            (getSum $ jmFailed metricsPrevious)
        unfinishedJobs = getSum $ jmUnfinished metrics
        unfinishedJobsPercent = percentage unfinishedJobs totalJobs
        unfinishedJobsChanged = changedPercentage
            (getSum $ jmUnfinished metrics)
            (getSum $ jmUnfinished metricsPrevious)

    pure $ JobStats { .. }

data Range
    = Week
    | Month
    | Year
    deriving stock (Eq, Show, Bounded, Enum)

rangeOptions :: [(Text, Range)]
rangeOptions = [("week", Week), ("month", Month), ("year", Year)]

rangeIndex :: HasCallStack => Range -> Int
rangeIndex range = maybe (error msg) (+ 1) $ elemIndex range $ map
    snd
    rangeOptions
  where
    msg = "rangeIndex: " <> show range <> " is not present in rangeOptions"

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

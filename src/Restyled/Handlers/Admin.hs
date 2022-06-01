{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Restyled.Handlers.Admin
    ( getAdminR
    , getAdminStatsReposR
    , getAdminStatsJobsR
    ) where

import Restyled.Prelude.Esqueleto

import Data.List (elemIndex)
import Formatting (format)
import Formatting.Formatters as Formatters
import Formatting.Time (diff)
import Lens.Micro (_1, _3)
import Restyled.DB
import Restyled.Foundation
import Restyled.Metrics
import Restyled.Models
import Restyled.Percentage
import Restyled.Routes
import Restyled.Settings
import Restyled.Time
import Restyled.TimeRange
import Restyled.UsCents
import Restyled.Yesod
import Text.Blaze (ToMarkup(..))

getAdminR :: Handler Html
getAdminR = do
    now <- liftIO getCurrentTime
    (trialMrrs, mrrs) <- partition (trialing now . snd) <$> runDB fetchMRR

    let trialMrr = sum $ map fst trialMrrs
        mrr = sum $ map fst mrrs

    adminLayout $ do
        setTitle "Restyled Admin"
        $(widgetFile "admin/dashboard")

trialing :: UTCTime -> Maybe UTCTime -> Bool
trialing asOf = \case
    Just endsAt | endsAt >= asOf -> True
    _ -> False

-- brittany-disable-next-binding

fetchMRR
    :: MonadIO m
    => SqlPersistT m [(UsCents, Maybe UTCTime)]
fetchMRR = selectMap unValue2 $ from $ \(plans `InnerJoin` accounts) -> do
    on $ accounts ^. MarketplaceAccountMarketplacePlan ==. plans ^. persistIdField
    groupBy
        ( plans ^. persistIdField
        , accounts ^. MarketplaceAccountTrialEndsAt
        )
    orderBy [asc $ accounts ^. MarketplaceAccountTrialEndsAt]
    pure
        ( plans ^. MarketplacePlanMonthlyRevenue *. count (accounts ^. persistIdField)
        , accounts ^. MarketplaceAccountTrialEndsAt
        )

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
    }

getAdminStatsReposR :: Handler Html
getAdminStatsReposR = do
    (range, timeRange) <- getAdminStatsTimeRange
    RepoStats {..} <- runDB $ fetchRepoStats timeRange
    let trange = show @Text range

    fragmentLayout [whamlet|
        <p>#{pluralizeWith Formatters.commas "Repo" "Repos" totalRepos}
        <p>#{pluralizeWith Formatters.commas "Unique Owner" "Unique Owners" uniqueOwners}
        <p>
            #{format Formatters.commas activeRepos} (#{activeReposPercent}) with Jobs this #{trange}
            ^{changedWidget activeReposChanged}
    |]

fetchRepoStats :: MonadIO m => TimeRange -> SqlPersistT m RepoStats
fetchRepoStats timeRange = do
    reposWithActivity <- fetchReposWithActivity timeRange
    reposWithActivityPrevious <- fetchReposWithActivity
        $ timeRangeBefore timeRange

    let totalRepos = length reposWithActivity
        activeRepos = length $ filter (view _3) reposWithActivity
        activeReposPercent = percentage activeRepos totalRepos
        activeReposChanged = changedPercentage
            (length $ filter (view _3) reposWithActivityPrevious)
            activeRepos
        uniqueOwners = length $ nubOrd $ map (view _1) reposWithActivity
    pure RepoStats { .. }

-- brittany-disable-next-binding

-- | Fetch All Owner/Name pairs, with number of Jobs during the 'TimeRange'
--
-- N.B. we're using @^.@ on the @jobs@ even though it's an @OUTER JOIN@ and so
-- it could be nullable. It's safe because we don't return any
-- possibly-@Nothing@ fields as non-@Maybe@ values to Haskell. And it makes it
-- much easier to work with (e.g. for 'withinTimeRange')
--
fetchReposWithActivity
    :: MonadIO m
    => TimeRange
    -> SqlPersistT m [(OwnerName, RepoName, Bool)]
fetchReposWithActivity timeRange =
    selectMap convert $ from $ \(repos `LeftOuterJoin` jobs) -> do
        on $ repos ^. RepoOwner ==. jobs ^. JobOwner
            &&. repos ^. RepoName ==. jobs ^. JobRepo
            &&. jobs ^. JobCreatedAt `withinTimeRange` timeRange
        groupBy (repos ^. RepoOwner, repos ^. RepoName)
        pure
            ( repos ^. RepoOwner
            , repos ^. RepoName
            , count @Int $ jobs ^. persistIdField
            )
    where convert = over _3 (> 0) . unValue3

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
    let trange = show @Text range

    fragmentLayout [whamlet|
        <p>
            #{pluralizeWith Formatters.commas "Job" "Jobs" totalJobs} this #{trange}
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
    = Day
    | Week
    | Month
    | Year
    deriving stock (Eq, Show, Bounded, Enum)

rangeOptions :: [(Text, Range)]
rangeOptions = [("day", Day), ("week", Week), ("month", Month), ("year", Year)]

rangeIndex :: HasCallStack => Range -> Int
rangeIndex range = maybe (error msg) (+ 1) $ elemIndex range $ map
    snd
    rangeOptions
  where
    msg = "rangeIndex: " <> show range <> " is not present in rangeOptions"

getAdminStatsTimeRange :: Handler (Range, TimeRange)
getAdminStatsTimeRange = do
    range <- fmap (fromMaybe Day) $ runInputGet $ iopt
        (selectFieldList rangeOptions)
        "range"

    timeRange <- timeRangeFromAgo $ case range of
        Day -> Days 1
        Week -> Days 7
        Month -> Days 31
        Year -> Days 365

    pure (range, timeRange)

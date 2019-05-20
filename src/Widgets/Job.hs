{-# LANGUAGE TemplateHaskell #-}

module Widgets.Job
    ( jobCard
    , jobOutput

    -- * Job completion
    -- |
    --
    -- Needed in @"Widgets.Repo"@
    --
    , Completion(..)
    , jobCompletion
    )
where

import Import

import qualified Data.Text as T
import Formatting (format)
import Formatting.Time (diff)
import Widgets.ContainsURLs

-- | Internal helper for rendering completion state
data Completion
    = Success UTCTime
    | Failure UTCTime Int
    | InProgress

jobCompletion :: Job -> Completion
jobCompletion job = case (jobCompletedAt job, jobExitCode job) of
    (Just completedAt, Just 0) -> Success completedAt
    (Just completedAt, Just n) -> Failure completedAt n
    _ -> InProgress

-- | Is this @'Job'@ retriable?
--
-- Retries are on a dedicate queue that doesn't have guards that it's an
-- acceptable Job (GitHub repo, no Plan limitations, etc). This could create a
-- path around said guards by retrying a @'Job'@ that was skipped because of
-- them.
--
-- To avoid this, we should only allow retries on @'Job'@s that we know we've
-- accepted once already.
--
-- At this time, skipping a @'Job'@ marks it successful; it's also unlikely
-- retrying a successful @'Job'@ is useful. Therefore, @'jobIsRetriable' checks
-- that the @'Job'@ is complete and errored as a proxy.
--
-- NB. We further rely on the fact that we won't have an exit code for
-- incomplete @'Job'@, so we don't explicitly check @'jobCompletedAt'@.
--
jobIsRetriable :: Job -> Bool
jobIsRetriable = (`notElem` [Nothing, Just 0]) . jobExitCode

jobCard :: Entity Job -> Widget
jobCard job = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/job-card")

-- brittany-disable-next-binding

jobOutput :: Entity Job -> Widget
jobOutput (Entity jobId job) = $(widgetFile "widgets/job-output")

colorizedLogLine :: Text -> Text -> Widget
colorizedLogLine stream ln
    | Just message <- T.stripPrefix "[Debug] " ln = logLine "debug" message
    | Just message <- T.stripPrefix "[Info] " ln = logLine "info" message
    | Just message <- T.stripPrefix "[Warn] " ln = logLine "warn" message
    | Just message <- T.stripPrefix "[Error] " ln = logLine "error" message
    | otherwise = logLine "unknown" ln
  where
    logLine :: Text -> Text -> Widget
    logLine level message = $(widgetFile "widgets/log-line")

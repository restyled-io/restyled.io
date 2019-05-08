{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
    ) where

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

jobIsComplete :: Job -> Bool
jobIsComplete = isJust . jobCompletedAt

jobCard :: Entity Job -> Widget
jobCard job = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/job-card")
  where
    rerunR =
        let Entity jobId Job {..} = job
        in repoP jobOwner jobRepo $ RepoJobsP $ RepoJobP jobId RepoJobRetryR

-- brittany-disable-next-binding

jobOutput :: Job -> Widget
jobOutput job = $(widgetFile "widgets/job-output")

colorizedLogLine :: Text -> Widget
colorizedLogLine ln
    | Just message <- T.stripPrefix "[Debug] " ln = logLine "debug" message
    | Just message <- T.stripPrefix "[Info] " ln = logLine "info" message
    | Just message <- T.stripPrefix "[Warn] " ln = logLine "warn" message
    | Just message <- T.stripPrefix "[Error] " ln = logLine "error" message
    | otherwise = logLine "unknown" ln
  where
    logLine :: Text -> Text -> Widget
    logLine level message = $(widgetFile "widgets/log-line")

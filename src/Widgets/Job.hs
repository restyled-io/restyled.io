{-# LANGUAGE TemplateHaskell #-}

module Widgets.Job
    ( jobCard
    , jobOutput

    -- * Job completion
    , Completion(..)
    , jobCompletion

    -- * Utilities
    , colorizedJobLogLine
    )
where

import Import

import qualified Data.Text as T
import Formatting (format)
import Formatting.Time (diff)
import Foundation
import Routes
import Text.Julius (RawJS(..))
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

jobCard :: (Entity Job, JobOutput) -> Widget
jobCard (Entity jobId job, output) = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/job-card")

jobOutput :: JobOutput -> Widget
jobOutput output = $(widgetFile "widgets/job-output")
  where
    streamElementId = case output of
        JobOutputInProgress (Entity jobId _) ->
            "logs-job-id-" <> toPathPiece jobId
        _ -> "unused"

colorizedJobLogLine :: Entity JobLogLine -> Widget
colorizedJobLogLine (Entity _ JobLogLine {..}) =
    colorizedLogLine jobLogLineStream jobLogLineContent

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

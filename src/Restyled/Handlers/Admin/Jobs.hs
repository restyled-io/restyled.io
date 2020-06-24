{-# LANGUAGE LambdaCase #-}

module Restyled.Handlers.Admin.Jobs
    ( getAdminJobsR
    )
where

import Restyled.Prelude hiding (to)

import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.TimeRange
import Restyled.Yesod

data JobSummary = JobSummary (Route App -> Text) (Entity Job)

instance ToJSON JobSummary where
    toJSON (JobSummary urlRender (Entity jobId Job {..})) = object
        [ "created" .= jobCreatedAt
        , "repoPull" .= repoPullPath jobOwner jobRepo jobPullRequest
        , "exitCode" .= jobExitCode
        , "exitReason" .= (reasonForExitCode =<< jobExitCode)
        , "url" .= urlRender (repoP jobOwner jobRepo $ jobR jobId)
        ]

data JobsFilter
    = JobsFilterAll
    | JobsFilterUnfinished
    | JobsFilterErrored

optionalJobsFilter :: Handler JobsFilter
optionalJobsFilter = fromMaybeM (pure JobsFilterAll) $ runInputGet $ iopt
    (eitherField readJobsFilter)
    "filter"

readJobsFilter :: Text -> Either Text JobsFilter
readJobsFilter = \case
    "all" -> Right JobsFilterAll
    "unfinished" -> Right JobsFilterUnfinished
    "errored" -> Right JobsFilterErrored
    x -> Left $ "Invalid filter: " <> x

getAdminJobsR :: Handler Value
getAdminJobsR = do
    range <- requiredTimeRange
    jobsFilter <- optionalJobsFilter
    jobs <- runDB $ selectListWithTimeRange JobCreatedAt range
    urlRender <- getUrlRender
    sendResponse $ toJSON $ map (JobSummary urlRender) $ case jobsFilter of
        JobsFilterAll -> jobs
        JobsFilterUnfinished ->
            filter (isNothing . jobExitCode . entityVal) jobs
        JobsFilterErrored ->
            filter (maybe False (/= 0) . jobExitCode . entityVal) jobs

-- | Give myself /something/ human readable
--
-- This is a maintenance burden to keep in sync, but the stakes are low. We'll
-- do our best.
--
-- <https://github.com/restyled-io/restyler/blob/712fee7b5cc9d823ce99fd3a2d0de96d3e35b78f/src/Restyler/App/Error.hs#L166>
--
reasonForExitCode :: Int -> Maybe Text
reasonForExitCode = \case
    0 -> Nothing
    10 -> Just "Invalid YAML"
    11 -> Just "Unknown Restyler"
    12 -> Just "Invalid restylers.yaml"
    20 -> Just "Restyler error"
    25 -> Just "Restyle error"
    30 -> Just "GitHub error"
    31 -> Just "PR fetch failure"
    32 -> Just "PR clone failure"
    40 -> Just "HTTP Error"
    50 -> Just "System error"
    99 -> Just "Known unknown"
    _ -> Just "Unknown unknown"

{-# LANGUAGE LambdaCase #-}

module Restyled.Handlers.Admin.Jobs
    ( getAdminJobsR
    )
where

import Restyled.Prelude hiding (to)

import Data.Semigroup (Arg(..))
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.TimeRange
import Restyled.Yesod
import RIO.List (sort)

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
    | JobsFilterSlow
    | JobsFilterErrored

requiredJobsFilter :: Handler JobsFilter
requiredJobsFilter = runInputGet $ ireq (eitherField readJobsFilter) "filter"

readJobsFilter :: Text -> Either Text JobsFilter
readJobsFilter = \case
    "all" -> Right JobsFilterAll
    "unfinished" -> Right JobsFilterUnfinished
    "slow" -> Right JobsFilterSlow
    "errored" -> Right JobsFilterErrored
    x -> Left $ "Invalid filter: " <> x

getAdminJobsR :: Handler Value
getAdminJobsR = do
    range <- requiredTimeRange
    jobsFilter <- requiredJobsFilter
    urlRender <- getUrlRender

    let
        selectJobs filters =
            selectList (timeRangeFilter JobCreatedAt range <> filters)

    jobs <- runDB $ case jobsFilter of
        JobsFilterAll -> selectJobs [] [Desc JobCreatedAt]
        JobsFilterUnfinished ->
            selectJobs [JobExitCode ==. Nothing] [Desc JobCreatedAt]
        JobsFilterSlow ->
            toSlowJobs <$> selectJobs [JobExitCode !=. Nothing] []
        JobsFilterErrored -> selectJobs
            [JobExitCode !=. Nothing, JobExitCode !=. Just 0]
            [Desc JobCompletedAt]

    sendResponse $ toJSON $ map (JobSummary urlRender) jobs

toSlowJobs :: [Entity Job] -> [Entity Job]
toSlowJobs = take 10 . map getArg . sort . mapMaybe (argByMay jobDuration)

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

argByMay :: (b -> Maybe a) -> b -> Maybe (Arg a b)
argByMay f b = (`Arg` b) <$> f b

getArg :: Arg a b -> b
getArg (Arg _ b) = b

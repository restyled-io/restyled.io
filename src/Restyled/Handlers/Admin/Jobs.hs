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
        , "url" .= urlRender (repoP jobOwner jobRepo $ jobR jobId)
        ]

getAdminJobsR :: Handler Value
getAdminJobsR = do
    range <- requiredTimeRange
    jobs <- runDB $ selectListWithTimeRange JobCompletedAt range
    urlRender <- getUrlRender
    sendResponse $ toJSON $ map (JobSummary urlRender) jobs

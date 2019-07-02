module Restyled.Handlers.Admin.Jobs
    ( getAdminJobsR
    )
where

import Restyled.Prelude hiding (to)

import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Yesod

data JobSummary = JobSummary (Route App -> Text) (Entity Job)

instance ToJSON JobSummary where
    toJSON (JobSummary urlRender (Entity jobId Job{..})) = object
        [ "created" .= jobCreatedAt
        , "repoPull" .= repoPullPath jobOwner jobRepo jobPullRequest
        , "exitCode" .= jobExitCode
        , "url" .= urlRender (repoP jobOwner jobRepo $ jobR jobId)
        ]

getAdminJobsR :: Handler Value
getAdminJobsR = do
    (from, to) <- runInputGet $ (,) <$> ireq epochField "from" <*> ireq
        epochField
        "to"

    jobs <- runDB $ selectList
        [JobCompletedAt >=. Just from, JobCompletedAt <=. Just to]
        [Desc JobCompletedAt]

    urlRender <- getUrlRender
    sendResponse $ toJSON $ map (JobSummary urlRender) jobs

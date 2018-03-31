{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Widgets.Job
    ( jobsTable
    , jobsTableRow

    -- * Creating Jobs
    , CreateJob(..)
    , createJobForm
    , createJobFormFrom
    ) where

import Import

import GitHub.Data
import GitHub.Data.Apps
import Helper.Time

data CreateJob = CreateJob
    { cjInstallationId :: Id Installation
    , cjOwner :: Name Owner
    , cjRepo :: Name Repo
    , cjPullRequest :: Id PullRequest
    }

-- | Form to use when rendering for real input, or processing
createJobForm :: Form CreateJob
createJobForm = renderDivs $ CreateJob
    <$> (mkId Proxy <$> areq intField "Installation Id" Nothing)
    <*> (mkName Proxy <$> areq textField "Owner" Nothing)
    <*> (mkName Proxy <$> areq textField "Repo" Nothing)
    <*> (mkId Proxy <$> areq intField "Pull Request" Nothing)

-- | Form to use when re-submitting an existing @'Job'@
createJobFormFrom :: Job -> Form CreateJob
createJobFormFrom Job{..} = renderDivs $ CreateJob
    <$> areq hiddenField "" (Just jobInstallationId)
    <*> areq hiddenField "" (Just jobOwner)
    <*> areq hiddenField "" (Just jobRepo)
    <*> areq hiddenField "" (Just jobPullRequest)

jobsTable :: [Entity Job] -> Widget
jobsTable jobs = $(widgetFile "widgets/jobs-table")

jobsTableRow :: Entity Job -> Widget
jobsTableRow job = do
    now <- liftIO getCurrentTime
    (widget, enctype) <- handlerToWidget
        $ generateFormPost
        $ createJobFormFrom
        $ entityVal job
    $(widgetFile "widgets/jobs-table-row")

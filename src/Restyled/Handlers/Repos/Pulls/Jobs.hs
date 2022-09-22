{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Repos.Pulls.Jobs
    ( getRepoPullJobsR
    ) where

import Restyled.Prelude

import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Paginate
import Restyled.Settings
import Restyled.Widgets.Job
import Restyled.Yesod

getRepoPullJobsR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullJobsR owner name num = do
    page <- runDB $ paginateBy
        JobCreatedAt
        Descending
        15
        [JobOwner ==. owner, JobRepo ==. name, JobPullRequest ==. num]

    defaultLayout $ do
        setTitle $ toHtml $ repoPullPath owner name num <> " jobs"
        $(widgetFile "jobs")

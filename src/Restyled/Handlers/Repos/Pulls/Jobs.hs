{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Repos.Pulls.Jobs
    ( getRepoPullJobsR
    ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Widgets.Job
import Restyled.Yesod

getRepoPullJobsR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullJobsR owner name num = do
    pages <- runDB $ selectPaginated
        15
        [JobOwner ==. owner, JobRepo ==. name, JobPullRequest ==. num]
        [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle $ toHtml $ repoPullPath owner name num <> " jobs"
        $(widgetFile "jobs")

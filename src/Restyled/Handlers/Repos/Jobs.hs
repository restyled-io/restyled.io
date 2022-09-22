{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Repos.Jobs
    ( getRepoJobsR
    , getRepoJobR
    ) where

import Restyled.Prelude

import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Paginate
import Restyled.Settings
import Restyled.Widgets.Job
import Restyled.Yesod

getRepoJobsR :: OwnerName -> RepoName -> Handler Html
getRepoJobsR owner name = do
    page <- runDB $ paginateBy
        JobCreatedAt
        Descending
        2
        [JobOwner ==. owner, JobRepo ==. name]

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: OwnerName -> RepoName -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ getEntity404 jobId

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " #" <> toPathPiece jobId
        $(widgetFile "job")

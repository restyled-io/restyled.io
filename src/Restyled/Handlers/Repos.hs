{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Repos
    ( getRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    , getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Foundation
import Restyled.JobLogLine
import Restyled.Models
import Restyled.Settings
import Restyled.StreamJobLogLines
import Restyled.Widgets.Job
import Restyled.Yesod
import Yesod.WebSockets

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

getRepoPullR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullR = getRepoPullJobsR

getRepoPullJobsR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullJobsR owner name num = do
    pages <- runDB $ traverse attachJobOutput =<< selectPaginated
        5
        [JobOwner ==. owner, JobRepo ==. name, JobPullRequest ==. num]
        [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle $ toHtml $ repoPullPath owner name num <> " jobs"
        $(widgetFile "jobs")

getRepoJobsR :: OwnerName -> RepoName -> Handler Html
getRepoJobsR owner name = do
    pages <- runDB $ traverse attachJobOutput =<< selectPaginated
        5
        [JobOwner ==. owner, JobRepo ==. name]
        [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: OwnerName -> RepoName -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    jobWithOutput <- runDB $ do
        job <- fromMaybeM notFound $ getEntity jobId
        attachJobOutput job

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " #" <> toPathPiece jobId
        $(widgetFile "job")

getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobLogLinesR _owner _name jobId = do
    void $ runDB $ getEntity404 jobId
    webSockets $ streamJobLogLines jobId

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobLogLines jobId
    pure $ T.unlines $ map textJobLogLine jobLogLines

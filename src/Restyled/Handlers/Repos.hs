{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Repos
    ( getRepoR
    , putRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    , getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import qualified Data.Text as T
import Restyled.Api.CreateRepo
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.StreamJobLogLines
import Restyled.Widgets.Job
import Restyled.Yesod
import Yesod.WebSockets

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

-- | Find or create a repository
--
-- - Request: 'ApiRepo'
-- - Response: 400 with 'ApiCreateRepoErrors' or 200 with 'ApiRepo'
--
putRepoR :: OwnerName -> RepoName -> Handler Value
putRepoR owner name = do
    body <- requireJsonBody
    result <- runDB $ runValidateT $ do
        assertOwnerName owner body *> assertRepoName name body
        findOrCreateRepo body
    either (sendStatusJSON status400) (sendStatusJSON status200) result

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
    job <- runDB $ getEntity404 jobId
    webSockets $ streamJobLogLines jobId

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- runDB $ entityVal <$$> fetchJobLog job
    pure $ T.unlines $ map textJobLogLine jobLogLines

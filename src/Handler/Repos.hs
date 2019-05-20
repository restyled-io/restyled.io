{-# LANGUAGE TemplateHaskell #-}

module Handler.Repos
    ( getRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    , getRepoJobLogLinesR
    , postRepoJobRetryR
    )
where

import Import

import Backend.Job
import StreamJobLogLines
import Widgets.Job
import Yesod.Paginator
import Yesod.WebSockets

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

getRepoPullR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullR = getRepoPullJobsR

getRepoPullJobsR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullJobsR owner name num = do
    pages <- runDB $ selectPaginated
        5
        [JobOwner ==. owner, JobRepo ==. name, JobPullRequest ==. num]
        [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle $ toHtml $ repoPullPath owner name num <> " jobs"
        $(widgetFile "jobs")

getRepoJobsR :: OwnerName -> RepoName -> Handler Html
getRepoJobsR owner name = do
    pages <- runDB $ selectPaginated
        5
        [JobOwner ==. owner, JobRepo ==. name]
        [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: OwnerName -> RepoName -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ fromMaybeM notFound $ getEntity jobId

    defaultLayout $ do
        setTitle $ toHtml $ repoPath owner name <> " #" <> toPathPiece jobId
        $(widgetFile "job")

getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler ()
getRepoJobLogLinesR _owner _name jobId = do
    void $ runDB $ get404 jobId
    webSockets $ streamJobLogLines jobId

postRepoJobRetryR :: OwnerName -> RepoName -> JobId -> Handler Html
postRepoJobRetryR owner name jobId = do
    job <- runDB $ insertJobRetry =<< get404 jobId
    runHandlerRIO $ enqueueRestylerJob job
    setMessage "Job enqueued"
    redirect $ repoP owner name $ jobR $ entityKey job

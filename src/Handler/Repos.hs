{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Repos
    ( getRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    )
where

import Import

import Widgets.Job

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

getRepoPullR :: OwnerName -> RepoName -> PullRequestId -> Handler Html
getRepoPullR = getRepoPullJobsR

getRepoPullJobsR :: OwnerName -> RepoName -> PullRequestId -> Handler Html
getRepoPullJobsR owner name num = do
    jobs <- runDB $ selectList
        [JobOwner ==. owner, JobRepo ==. name, JobPullRequest ==. num]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPiece owner
            <> "/"
            <> toPathPiece name
            <> "#"
            <> toPathPiece num
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobsR :: OwnerName -> RepoName -> Handler Html
getRepoJobsR owner name = do
    jobs <- runDB $ selectList
        [JobOwner ==. owner, JobRepo ==. name]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPiece owner
            <> "/"
            <> toPathPiece name
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: OwnerName -> RepoName -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ fromMaybeM notFound =<< getEntity jobId

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPiece owner
            <> "/"
            <> toPathPiece name
            <> " #"
            <> toPathPiece jobId
        $(widgetFile "job")

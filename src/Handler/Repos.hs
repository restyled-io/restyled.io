{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Repos
    ( getReposR
    , getRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    )
where

import Import

import Authorization
import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH
import Widgets.Job
import Widgets.Repo

getReposR :: Name Owner -> Handler Html
getReposR owner = do
    reposWithStats <- runDB $ do
        repos <- selectList
            [RepoOwner ==. owner]
            [Asc RepoName, LimitTo repositoriesListLimit]

        requireRepositoriesAccess repos
        traverse repoWithStats repos

    when (null reposWithStats) notFound

    defaultLayout $ do
        setTitle $ toHtml $ toPathPart owner <> " repositories"
        $(widgetFile "repos")

getRepoR :: Name Owner -> Name GH.Repo -> Handler Html
getRepoR = getRepoJobsR

getRepoPullR :: Name Owner -> Name GH.Repo -> Int -> Handler Html
getRepoPullR = getRepoPullJobsR

getRepoPullJobsR :: Name Owner -> Name GH.Repo -> Int -> Handler Html
getRepoPullJobsR owner name num = do
    jobs <- runDB $ selectList
        [ JobOwner ==. owner
        , JobRepo ==. name
        , JobPullRequest ==. mkId Proxy num
        ]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> "#"
            <> toPathPiece num
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobsR :: Name Owner -> Name GH.Repo -> Handler Html
getRepoJobsR owner name = do
    jobs <- runDB $ selectList
        [JobOwner ==. owner, JobRepo ==. name]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: Name Owner -> Name GH.Repo -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ fromMaybeM notFound =<< getEntity jobId

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " #"
            <> toPathPiece jobId
        $(widgetFile "job")

repositoriesListLimit :: Int
repositoriesListLimit = 50

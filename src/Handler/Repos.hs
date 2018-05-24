{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Repos
    ( getReposR
    , getRepoJobsR
    , getRepoJobR
    ) where

import Import

import Authorization
import Formatting (format)
import Formatting.Time (diff)
import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH
import Widgets.Job

getReposR :: Name Owner -> Handler Html
getReposR owner = do
    reposWithStats <- runDB $ do
        repos <- selectList
            [RepoOwner ==. owner]
            [Asc RepoName, LimitTo repositoriesListLimit]

        requireRepositoriesAccess repos
        traverse repoWithStats repos

    now <- liftIO getCurrentTime
    defaultLayout $ do
        setTitle $ toHtml $ toPathPart owner <> " repositories"
        $(widgetFile "repos")

getRepoJobsR :: Name Owner -> Name GH.Repo -> Handler Html
getRepoJobsR owner name = do
    jobs <- runDB $ do
        Entity _ repo <- getBy404 $ UniqueRepo owner name
        requireRepositoryAccess repo

        selectList
            [JobOwner ==. repoOwner repo, JobRepo ==. repoName repo]
            [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " jobs"
        $(widgetFile "repos/repo/jobs")

getRepoJobR :: Name Owner -> Name GH.Repo -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ do
        Entity _ repo <- getBy404 $ UniqueRepo owner name
        requireRepositoryAccess repo
        fromMaybeM notFound =<< getEntity jobId

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " #"
            <> toPathPiece jobId
        $(widgetFile "repos/repo/job")

repositoriesListLimit :: Int
repositoriesListLimit = 50

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Admin.Repos
    ( getAdminReposR
    , patchAdminRepoR
    , getAdminRepoJobsR
    ) where

import Import

import Widgets.Job
import Widgets.Repo

getAdminReposR :: Handler Html
getAdminReposR = do
    reposWithStats <- runDB $ do
        -- N.B. This will be problematic some day ;)
        repos <- selectList [] [Desc RepoId]
        traverse repoWithStats repos

    adminLayout $ do
        setTitle "Restyled Admin / Repos"
        $(widgetFile "admin/repos")

patchAdminRepoR :: RepoId -> Handler ()
patchAdminRepoR repoId = do
    debugEnabled <- runInputPost $ ireq boolField "debugEnabled"
    setMessage
        $ (if debugEnabled then "Enabled" else "Disabled")
        <> " debug for repository"
    runDB $ update repoId [RepoDebugEnabled =. debugEnabled]
    redirect $ AdminP $ AdminReposP AdminReposR

getAdminRepoJobsR :: RepoId -> Handler Html
getAdminRepoJobsR repoId = do
    (repo, jobs) <- runDB $ do
        repo@Repo {..} <- get404 repoId
        (repo, ) <$> selectList
            [JobOwner ==. repoOwner, JobRepo ==. repoName]
            [Desc JobCreatedAt]

    (widget, enctype) <- generateFormPost $ createJobFormFromRepo repo

    adminLayout $ do
        setTitle "Restyled Admin / Repo Jobs"
        $(widgetFile "admin/repos/jobs")

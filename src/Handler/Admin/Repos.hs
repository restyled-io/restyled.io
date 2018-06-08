{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Admin.Repos
    ( getAdminReposR
    , patchAdminRepoR
    , getAdminRepoJobsR
    )
where

import Import

import Widgets.Job
import Widgets.Repo
import Yesod.Paginator
import Yesod.Paginator.Instances ()

getAdminReposR :: Handler Html
getAdminReposR = do
    pages <- runDB $ do
        pages <- selectPaginated 5 [] [Desc RepoId]
        traverse repoWithStats pages

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Admin.Repos
    ( getAdminReposR
    , getAdminReposSearchR
    , patchAdminRepoR
    , getAdminRepoJobsR
    )
where

import Import

import Admin.RepoSearch
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

getAdminReposSearchR :: Handler TypedContent
getAdminReposSearchR = do
    mQuery <- runInputGet $ iopt textField "q"
    results <- maybe (pure noResults) (searchRepos 10) mQuery

    selectRep $ do
        provideRep $ pure $ toJSON results
        provideRep $ adminLayout $ do
            setTitle "Restyled Admin / Search"
            $(widgetFile "admin/repos/search")

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
    (repo, pages) <- runDB $ do
        repo@Repo {..} <- get404 repoId
        (repo, ) <$> selectPaginated
            5
            [JobOwner ==. repoOwner, JobRepo ==. repoName]
            [Desc JobCreatedAt]

    (widget, enctype) <- generateFormPost $ createJobFormFromRepo repo

    adminLayout $ do
        setTitle "Restyled Admin / Repo Jobs"
        $(widgetFile "admin/repos/jobs")

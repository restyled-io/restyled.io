{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Admin.Repos
    ( getAdminReposR
    , getAdminRepoJobsR
    ) where

import Import

import Widgets.Job

getAdminReposR :: Handler Html
getAdminReposR = do
    repos <- runDB $ selectList [] [Desc RepoId]

    adminLayout $ do
        setTitle "Restyled Admin / Repos"
        $(widgetFile "admin/repos")

getAdminRepoJobsR :: RepoId -> Handler Html
getAdminRepoJobsR repoId = do
    (repo, jobs) <- runDB $ do
        repo@Repo{..} <- get404 repoId
        (repo,) <$> selectList
            [ JobOwner ==. repoOwner
            , JobRepo ==. repoName
            ]
            [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Repo Jobs"
        $(widgetFile "admin/repos/jobs")

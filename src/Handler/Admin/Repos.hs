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

import Data.Time
import Formatting (format)
import Formatting.Time (diff)
import Widgets.Job

data RepoWithStats = RepoWithStats
    { rwsRepo :: Entity Repo
    , rwsJobCount :: Int
    , rwsErrorCount :: Int
    , rwsLastJob :: Maybe (Entity Job)
    }

getAdminReposR :: Handler Html
getAdminReposR = do
    -- Naive. A. F.
    reposWithStats <- runDB $ do
        repos <- selectList [] [Desc RepoId]

        for repos $ \repo -> RepoWithStats repo
            <$> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                ]
            <*> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                , JobExitCode !=. Just 0
                , JobExitCode !=. Nothing
                ]
            <*> selectFirst
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                ]
                [ Desc JobCreatedAt
                ]

    now <- liftIO getCurrentTime
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

    adminLayout $ do
        setTitle "Restyled Admin / Repo Jobs"
        $(widgetFile "admin/repos/jobs")

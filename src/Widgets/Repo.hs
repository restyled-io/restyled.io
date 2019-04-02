{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.Repo
    ( adminRepoCard
    )
where

import Import

import Formatting (format)
import Formatting.Time (diff)
import Widgets.Job

adminRepoCard :: RepoWithStats -> Widget
adminRepoCard RepoWithStats {..} = do
    now <- liftIO getCurrentTime

    let jobsRoute :: Entity Repo -> Route App
        jobsRoute (Entity repoId _) = adminRepoP repoId AdminRepoJobsR

    $(widgetFile "widgets/repo-card")

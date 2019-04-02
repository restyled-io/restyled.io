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
adminRepoCard RepoWithStats{..} = do
    now <- liftIO getCurrentTime

    let
        jobsRoute :: Entity Repo -> Route App
        jobsRoute (Entity repoId _) = adminRepoP repoId AdminRepoJobsR

        mAction :: Maybe Widget
        mAction = Just $ adminRepoActions rwsRepo

    $(widgetFile "widgets/repo-card")

adminRepoActions :: Entity Repo -> Widget
adminRepoActions repo = [whamlet|
    <form method=post action=@{adminRepoR $ entityKey repo}>
        <input type=hidden name=_method value=PATCH />
        $if repoDebugEnabled $ entityVal repo
          <input type=hidden name=debugEnabled value=no />
          <input type=submit value="Disable debug" />
        $else
          <input type=hidden name=debugEnabled value=yes />
          <input .action type=submit value="Enable debug" />
|]

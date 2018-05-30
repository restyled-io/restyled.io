{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.Repo
    ( repoCard
    , adminRepoCard
    ) where

import Import

import qualified Data.Text.Lazy as TL
import Formatting (format, (%))
import Formatting.Formatters (int, plural)
import Formatting.Time (diff)
import Widgets.Job

repoCard :: RepoWithStats -> Widget
repoCard RepoWithStats{..} = do
    now <- liftIO getCurrentTime

    let
        jobsRoute :: Entity Repo -> Route App
        jobsRoute = repoJobsRoute

        mAction :: Maybe Widget
        mAction = Nothing

    $(widgetFile "widgets/repo-card")

adminRepoCard :: RepoWithStats -> Widget
adminRepoCard RepoWithStats{..} = do
    now <- liftIO getCurrentTime

    let
        jobsRoute :: Entity Repo -> Route App
        jobsRoute = adminRepoJobsRoute

        mAction :: Maybe Widget
        mAction = Just $ adminRepoActions rwsRepo

    $(widgetFile "widgets/repo-card")

adminRepoActions :: Entity Repo -> Widget
adminRepoActions repo = [whamlet|
    <form method=post action=@{adminRepoRoute repo}>
        <input type=hidden name=_method value=PATCH />
        $if repoDebugEnabled $ entityVal repo
          <input type=hidden name=debugEnabled value=no />
          <input type=submit value="Disable debug" />
        $else
          <input type=hidden name=debugEnabled value=yes />
          <input .action type=submit value="Enable debug" />
|]

pluralize :: TL.Text -> TL.Text -> Int -> TL.Text
pluralize s p n = format (int % " " % plural s p) n n

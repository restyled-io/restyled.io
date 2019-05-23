{-# LANGUAGE TemplateHaskell #-}

module Widgets.Repo
    ( repoCard
    )
where

import Import

import Formatting (format)
import Formatting.Time (diff)
import Foundation
import Routes
import Widgets.Job

repoCard :: RepoWithStats -> Widget
repoCard RepoWithStats {..} = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/repo-card")

{-# LANGUAGE TemplateHaskell #-}

module Restyled.Widgets.Repo
    ( repoCard
    )
where

import Restyled.Prelude

import Formatting (format)
import Formatting.Time (diff)
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.Widgets.Job

repoCard :: RepoWithStats -> Widget
repoCard RepoWithStats {..} = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/repo-card")

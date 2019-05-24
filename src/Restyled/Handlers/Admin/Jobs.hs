{-# LANGUAGE TemplateHaskell #-}

-- | List all Jobs in the system
module Restyled.Handlers.Admin.Jobs
    ( getAdminJobsR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Widgets.Job
import Restyled.Yesod

getAdminJobsR :: Handler Html
getAdminJobsR = do
    pages <- runDB $ traverse attachJobOutput =<< selectPaginated
        5
        []
        [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Jobs"
        $(widgetFile "jobs")

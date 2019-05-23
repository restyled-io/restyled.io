{-# LANGUAGE TemplateHaskell #-}

-- | List all Jobs in the system
module Handler.Admin.Jobs
    ( getAdminJobsR
    )
where

import Import

import Foundation
import Widgets.Job
import Yesod
import Yesod.Paginator

getAdminJobsR :: Handler Html
getAdminJobsR = do
    pages <- runDB $ traverse attachJobOutput =<< selectPaginated
        5
        []
        [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Jobs"
        $(widgetFile "jobs")

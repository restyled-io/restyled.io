{-# LANGUAGE TemplateHaskell #-}

-- | List all Jobs in the system
module Handler.Admin.Jobs
    ( getAdminJobsR
    )
where

import Import

import Widgets.Job
import Yesod.Paginator

getAdminJobsR :: Handler Html
getAdminJobsR = do
    pages <- runDB $ selectPaginated 5 [] [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Jobs"
        $(widgetFile "jobs")

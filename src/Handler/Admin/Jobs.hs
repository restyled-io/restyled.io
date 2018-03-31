{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs
    ( getAdminJobsR
    ) where

import Import

import Widgets.Job

getAdminJobsR :: Handler Html
getAdminJobsR = do
    jobs <- runDB $ selectList [] [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Jobs"
        $(widgetFile "admin/jobs")

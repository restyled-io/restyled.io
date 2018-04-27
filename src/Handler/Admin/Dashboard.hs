{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Dashboard
    ( getAdminR
    , getAdminDashboardR
    ) where

import Import

import Widgets.Job

getAdminR :: Handler Html
getAdminR = redirect $ AdminP AdminDashboardR

getAdminDashboardR :: Handler Html
getAdminDashboardR = do
    recentJobs <- runDB $
        selectList [] [Desc JobCreatedAt, LimitTo 5]

    adminLayout $ do
        setTitle "Restyled Admin"
        $(widgetFile "admin/dashboard")

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Dashboard
    ( getAdminR
    , getAdminDashboardR
    ) where

import Import

data Dashboard = Dashboard
    { dLatestJobAt :: Maybe UTCTime
    , dIncompleteJobs :: Int
    , dCompleteJobs :: Int
    , dErroredJobs :: Int
    }

getAdminR :: Handler Html
getAdminR = redirect $ AdminP AdminDashboardR

getAdminDashboardR :: Handler Html
getAdminDashboardR = do
    dashboard <- runDB $ do
        mjob <- selectFirst
            [JobCompletedAt !=. Nothing]
            [Desc JobCompletedAt]

        Dashboard
            <$> pure (jobCompletedAt . entityVal =<< mjob)
            <*> count [JobCompletedAt ==. Nothing]
            <*> count [JobCompletedAt !=. Nothing]
            <*> count [JobCompletedAt !=. Nothing, JobExitCode !=. Just 0]

    defaultLayout $ do
        setTitle "Admin - Dashboard"
        $(widgetFile "admin/dashboard")

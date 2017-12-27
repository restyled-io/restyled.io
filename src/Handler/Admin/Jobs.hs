{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs where

import Import

import GitHub.Data (toPathPart)
import Helper.Time

getAdminJobsR :: Handler Html
getAdminJobsR = do
    jobs <- runDB $ selectList [] [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle "Admin - Jobs"
        $(widgetFile "admin/jobs")

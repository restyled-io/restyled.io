{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs where

import Import

import Data.Time (diffUTCTime)
import GitHub.Data (toPathPart)

getAdminJobsR :: Handler Html
getAdminJobsR = do
    jobs <- runDB $ selectList [] [Desc JobCreatedAt]

    defaultLayout $ do
        setTitle "Admin - Jobs"
        $(widgetFile "admin/jobs")

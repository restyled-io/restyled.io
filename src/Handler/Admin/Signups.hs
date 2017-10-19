{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Admin.Signups where

import           Import

getAdminSignupsR :: Handler Html
getAdminSignupsR = do
    signups <- runDB $ selectList [] []

    defaultLayout $ do
        setTitle "Admin - Signups"
        $(widgetFile "admin/signups")

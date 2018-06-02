{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Signups
    ( getAdminSignupsR
    )
where

import Import

getAdminSignupsR :: Handler Html
getAdminSignupsR = do
    signups <- runDB $ selectList [] []

    adminLayout $ do
        setTitle "Restyled Admin / Signups"
        $(widgetFile "admin/signups")

{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin
    ( getAdminR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Settings
import Restyled.Yesod

getAdminR :: Handler Html
getAdminR = adminLayout $ do
    setTitle "Restyled Admin"
    $(widgetFile "admin/dashboard")

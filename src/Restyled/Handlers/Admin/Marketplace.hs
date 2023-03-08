{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Marketplace
    ( getAdminMarketplaceR
    ) where

import Restyled.Prelude

import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.UsCents
import Restyled.Yesod

getAdminMarketplaceR :: Handler Html
getAdminMarketplaceR = do
    planCounts <- runDB fetchMarketplacePlans
    adminLayout $ do
        setTitle "Admin - Marketplace"
        $(widgetFile "admin/marketplace")

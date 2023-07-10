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
  planCounts <- filter (uncurry showPlan) <$> runDB fetchMarketplacePlans

  adminLayout $ do
    setTitle "Admin - Marketplace"
    $(widgetFile "admin/marketplace")
 where
  showPlan :: Entity MarketplacePlan -> Int -> Bool
  showPlan (Entity _ MarketplacePlan {..}) nAccounts =
    not $ marketplacePlanRetired && nAccounts == 0

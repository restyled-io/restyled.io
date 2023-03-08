module Restyled.Models.Marketplace
    ( fetchMarketplacePlans
    ) where

import Restyled.Prelude.Esqueleto

import Lens.Micro (_2)
import Restyled.Models.DB

fetchMarketplacePlans
    :: MonadIO m => SqlPersistT m [(Entity MarketplacePlan, Int)]
fetchMarketplacePlans =
    selectMap (over _2 unValue) $ from $ \(plans `InnerJoin` accounts) -> do
        on
            $ accounts
            ^. MarketplaceAccountMarketplacePlan
            ==. plans
            ^. persistIdField

        let nAccounts :: SqlExpr (Value Int)
            nAccounts = count $ accounts ^. persistIdField

        groupBy $ plans ^. persistIdField

        orderBy
            [ asc $ plans ^. MarketplacePlanRetired
            , desc $ plans ^. MarketplacePlanMonthlyRevenue
            , asc $ plans ^. MarketplacePlanName
            , desc nAccounts
            ]

        pure (plans, nAccounts)

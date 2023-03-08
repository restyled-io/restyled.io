module Restyled.Models.Marketplace
    ( fetchMarketplacePlans
    , fetchUniqueOwnersWithoutPlan
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
            , desc nAccounts
            , asc $ plans ^. MarketplacePlanName
            ]

        pure (plans, nAccounts)

fetchUniqueOwnersWithoutPlan :: MonadIO m => SqlPersistT m [(OwnerName, Int)]
fetchUniqueOwnersWithoutPlan =
    selectMap unValue2 $ from $ \(repos `LeftOuterJoin` accounts) -> do
        on $ accounts ?. MarketplaceAccountGithubLogin `stringEqual` just
            (repos ^. RepoOwner)
        where_ $ isNothing $ accounts ?. persistIdField

        groupBy $ repos ^. RepoOwner

        let nRepos :: SqlExpr (Value Int)
            nRepos = count $ repos ^. persistIdField

        orderBy [desc nRepos, asc $ repos ^. RepoOwner]

        pure (repos ^. RepoOwner, nRepos)

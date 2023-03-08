module Restyled.Models.Marketplace
    ( fetchMarketplacePlans
    , fetchMarketplaceAccountsWithPlan
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

fetchMarketplaceAccountsWithPlan
    :: MonadIO m
    => MarketplacePlanId
    -> SqlPersistT m [(Entity MarketplaceAccount, Int)]
fetchMarketplaceAccountsWithPlan planId =
    selectMap (over _2 unValue) $ from $ \(repos `InnerJoin` accounts) -> do
        on
            $ accounts
            ^. MarketplaceAccountGithubLogin
            `stringEqual` repos
            ^. RepoOwner
        where_ $ accounts ^. MarketplaceAccountMarketplacePlan ==. val planId
        (_, n) <- groupCountBy accounts MarketplaceAccountGithubLogin
        pure (accounts, n)

fetchUniqueOwnersWithoutPlan :: MonadIO m => SqlPersistT m [(OwnerName, Int)]
fetchUniqueOwnersWithoutPlan =
    selectMap unValue2 $ from $ \(repos `LeftOuterJoin` accounts) -> do
        on $ accounts ?. MarketplaceAccountGithubLogin `stringEqual` just
            (repos ^. RepoOwner)
        where_ $ isNothing $ accounts ?. persistIdField
        groupCountBy repos RepoOwner

groupCountBy
    :: (PersistEntity e, PersistField a)
    => SqlExpr (Entity e)
    -> EntityField e a
    -> SqlQuery (SqlExpr (Value a), SqlExpr (Value Int))
groupCountBy ent field = do
    groupBy $ ent ^. field
    orderBy [desc n, asc $ ent ^. field]
    pure (ent ^. field, n)
    where n = count $ ent ^. persistIdField

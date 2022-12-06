{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Restyled.Handlers.Admin.Marketplace
    ( getAdminMarketplaceR
    ) where

import Restyled.Prelude

import Data.List (nub)
import Restyled.DB
import Restyled.Foundation
import Restyled.Marketplace (isPrivateRepoPlan)
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.UsCents
import Restyled.Yesod

data MarketplacePlanWithAccounts = MarketplacePlanWithAccounts
    { mpwaPlan :: Entity MarketplacePlan
    , mpwaAccounts :: [Entity MarketplaceAccount]
    }

mpwaDescription :: MarketplacePlanWithAccounts -> Text
mpwaDescription = marketplacePlanDescription . entityVal . mpwaPlan

mpwaOwnerNames :: MarketplacePlanWithAccounts -> [OwnerName]
mpwaOwnerNames = map (accountOwner . entityVal) . mpwaAccounts
  where
    accountOwner :: MarketplaceAccount -> OwnerName
    accountOwner = nameToName . marketplaceAccountGithubLogin

mpwaOwnerAccounts
    :: UTCTime
    -> MarketplacePlanWithAccounts
    -> [(OwnerName, Maybe MarketplaceAccountId, Bool)]
mpwaOwnerAccounts now =
    map
            (\(Entity accountId MarketplaceAccount {..}) ->
                ( nameToName marketplaceAccountGithubLogin
                , Just accountId
                , maybe False (<= now) marketplaceAccountExpiresAt
                )
            )
        . mpwaAccounts

mpwaMonthlyRevenue :: MarketplacePlanWithAccounts -> UsCents
mpwaMonthlyRevenue MarketplacePlanWithAccounts {..} =
    genericLength mpwaAccounts
        * marketplacePlanMonthlyRevenue (entityVal mpwaPlan)

getAdminMarketplaceR :: Handler Html
getAdminMarketplaceR = do
    now <- getCurrentTime
    (plans, noPlanRepoOwners) <- runDB $ do
        plans' <- selectList
            []
            [ Asc MarketplacePlanRetired
            , Desc MarketplacePlanMonthlyRevenue
            , Asc MarketplacePlanName
            ]
        plans <- for plans' $ \plan ->
            MarketplacePlanWithAccounts plan <$> selectList
                [MarketplaceAccountMarketplacePlan ==. entityKey plan]
                [Asc MarketplaceAccountGithubId]

        let planOwners = concatMap mpwaOwnerNames plans
        (plans, )
            . map (, Nothing, False)
            <$> fetchUniqueRepoOwnersExcept planOwners

    adminLayout $ do
        setTitle "Admin - Marketplace"
        $(widgetFile "admin/marketplace")

fetchUniqueRepoOwnersExcept
    :: MonadIO m => [OwnerName] -> SqlPersistT m [OwnerName]
fetchUniqueRepoOwnersExcept exceptOwners = do
    owners <- repoOwner . entityVal <$$> selectList
        [RepoOwner /<-. exceptOwners]
        [Asc RepoOwner]
    pure $ nub owners

-- brittany-disable-next-binding

accountsList
    :: Maybe Text -> [(OwnerName, Maybe MarketplaceAccountId, Bool)] -> Widget
accountsList mDescription allOwners = do
    $(widgetFile "admin/marketplace/accounts-list")
  where
    (expiredOwners, owners) = partition (\(_, _, x) -> x) allOwners

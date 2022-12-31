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

data BriefAccountOwner = BriefAccountOwner
    { baoName :: OwnerName
    , baoAccountId :: Maybe MarketplaceAccountId
    , baoExpired :: Bool
    }

briefAccountOwner :: UTCTime -> Entity MarketplaceAccount -> BriefAccountOwner
briefAccountOwner now (Entity accountId MarketplaceAccount {..}) =
    BriefAccountOwner
        { baoName = nameToName marketplaceAccountGithubLogin
        , baoAccountId = Just accountId
        , baoExpired = maybe False (<= now) marketplaceAccountExpiresAt
        }

nonAccountOwner :: OwnerName -> BriefAccountOwner
nonAccountOwner name = BriefAccountOwner
    { baoName = name
    , baoAccountId = Nothing
    , baoExpired = False
    }

mpwaOwnerAccounts
    :: UTCTime -> MarketplacePlanWithAccounts -> [BriefAccountOwner]
mpwaOwnerAccounts now = map (briefAccountOwner now) . mpwaAccounts

mpwaMonthlyRevenue :: MarketplacePlanWithAccounts -> UsCents
mpwaMonthlyRevenue MarketplacePlanWithAccounts {..} =
    genericLength mpwaAccounts
        * marketplacePlanMonthlyRevenue (entityVal mpwaPlan)

getAdminMarketplaceR :: Handler Html
getAdminMarketplaceR = do
    now <- getCurrentTime
    (plans, noPlanRepoOwners) <- runDB $ do
        plans <-
            map (uncurry MarketplacePlanWithAccounts)
                <$> fetchMarketplacePlanAccounts

        let planOwners = concatMap mpwaOwnerNames plans

        (plans, )
            . map nonAccountOwner
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
    :: Maybe Text -> [BriefAccountOwner] -> Widget
accountsList mDescription allOwners = do
    $(widgetFile "admin/marketplace/accounts-list")
  where
    (expiredOwners, owners) = partition baoExpired allOwners

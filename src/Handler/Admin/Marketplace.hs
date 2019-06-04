{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Marketplace
    ( getAdminMarketplaceR
    )
where

import Import

import Backend.Marketplace (isPrivateRepoPlan)
import Data.List (nub)
import Foundation
import Yesod
import Yesod.Paginator

data MarketplacePlanWithAccounts = MarketplacePlanWithAccounts
    { mpwaPlan :: Entity MarketplacePlan
    , mpwaAccounts :: [Entity MarketplaceAccount]
    }

getAdminMarketplaceR :: Handler Html
getAdminMarketplaceR = do
    (pages, noPlanRepoOwners) <- runDB $ do
        plans <- selectPaginated 5 [] [Asc MarketplacePlanGithubId]
        pages <- for plans $ \plan ->
            MarketplacePlanWithAccounts plan <$> selectList
                [MarketplaceAccountMarketplacePlan ==. entityKey plan]
                [Asc MarketplaceAccountGithubId]

        let
            planOwners =
                marketplacePlanWithAccountsOwners $ pageItems $ pagesCurrent
                    pages

        (pages, ) <$> fetchUniqueRepoOwnersExcept planOwners

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

marketplacePlanWithAccountsOwners
    :: [MarketplacePlanWithAccounts] -> [OwnerName]
marketplacePlanWithAccountsOwners =
    map (marketplaceAccountGithubOwner . entityVal) . concatMap mpwaAccounts

marketplaceAccountGithubOwner :: MarketplaceAccount -> OwnerName
marketplaceAccountGithubOwner = nameToName . marketplaceAccountGithubLogin

accountsList :: [OwnerName] -> Widget
accountsList owners = $(widgetFile "admin/marketplace/accounts-list")

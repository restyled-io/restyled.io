{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Restyled.Handlers.Admin.Marketplace
    ( getAdminMarketplaceR
    , getAdminMarketplacePlansR
    , getAdminMarketplaceAccountsR
    , patchAdminMarketplaceAccountR
    )
where

import Restyled.Prelude

import Data.List (genericLength, nub)
import Restyled.Foundation
import Restyled.Marketplace (isPrivateRepoPlan)
import Restyled.Models
import Restyled.Settings
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

mpwaMonthlyRevenue :: MarketplacePlanWithAccounts -> UsCents
mpwaMonthlyRevenue MarketplacePlanWithAccounts {..} =
    genericLength mpwaAccounts
        * marketplacePlanMonthlyRevenue (entityVal mpwaPlan)

getAdminMarketplaceR :: Handler Html
getAdminMarketplaceR = do
    (plans, noPlanRepoOwners) <- runDB $ do
        plans' <- selectList [] [Asc MarketplacePlanGithubId]
        plans <- for plans' $ \plan ->
            MarketplacePlanWithAccounts plan <$> selectList
                [MarketplaceAccountMarketplacePlan ==. entityKey plan]
                [Asc MarketplaceAccountGithubId]

        let planOwners = concatMap mpwaOwnerNames plans
        (plans, ) <$> fetchUniqueRepoOwnersExcept planOwners

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

accountsList :: Maybe Text -> [OwnerName] -> Widget
accountsList mDescription owners =
    $(widgetFile "admin/marketplace/accounts-list")

newtype MarketplacePlansQuery = MarketplacePlansQuery
    { mpqGitHubId :: Int
    }

getAdminMarketplacePlansR :: Handler Value
getAdminMarketplacePlansR = do
    MarketplacePlansQuery {..} <- runInputGet $ MarketplacePlansQuery <$> ireq
        intField
        "githubId"

    let
        -- Pass githubId=-1 to query for non-GH Plans. TODO: Custom Field?
        githubId = case mpqGitHubId of
            -1 -> Nothing
            x -> Just x

    runDB $ toJSON <$> selectList
        [MarketplacePlanGithubId ==. githubId]
        [Asc MarketplacePlanName]

newtype MarketplaceAccountsQuery = MarketplaceAccountsQuery
    { maqGithubLogin :: GitHubUserName
    }

getAdminMarketplaceAccountsR :: Handler Value
getAdminMarketplaceAccountsR = do
    MarketplaceAccountsQuery {..} <-
        runInputGet
        $ MarketplaceAccountsQuery
        <$> (mkUserName <$> ireq textField "githubLogin")

    runDB $ toJSON <$> selectList
        [MarketplaceAccountGithubLogin ==. maqGithubLogin]
        [Asc MarketplaceAccountGithubLogin]

newtype MarketplaceAccountPatch = MarketplaceAccountPatch
    { mapMarketplacePlan :: Maybe MarketplacePlanId
    }

instance FromJSON MarketplaceAccountPatch where
    parseJSON = withObject "MarketplaceAccountPatch"
        $ \o -> MarketplaceAccountPatch <$> o .: "marketplacePlan"

patchAdminMarketplaceAccountR :: MarketplaceAccountId -> Handler Value
patchAdminMarketplaceAccountR accountId = do
    MarketplaceAccountPatch {..} <- requireCheckJsonBody

    let
        updates =
            [(MarketplaceAccountMarketplacePlan =.) <$> mapMarketplacePlan]

    runDB $ do
        update accountId $ catMaybes updates
        toJSON <$> get404 accountId

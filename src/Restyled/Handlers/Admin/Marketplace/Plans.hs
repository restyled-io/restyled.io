{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Marketplace.Plans
    ( getAdminMarketplacePlansR
    , getAdminMarketplacePlansNoneR
    , getAdminMarketplacePlanR
    ) where

import Restyled.Prelude

import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.UsCents
import Restyled.Yesod

newtype MarketplacePlansQuery = MarketplacePlansQuery
    { mpqGitHubId :: Int
    }

queryForm :: FormInput Handler MarketplacePlansQuery
queryForm = MarketplacePlansQuery <$> ireq intField "githubId"

queryFilters :: MarketplacePlansQuery -> [Filter MarketplacePlan]
queryFilters MarketplacePlansQuery {..} =
    [MarketplacePlanGithubId ==. githubId]
  where
    -- Pass githubId=-1 to query for non-GH Plans
    githubId = case mpqGitHubId of
        -1 -> Nothing
        x -> Just x

querySelectOpts :: MarketplacePlansQuery -> [SelectOpt MarketplacePlan]
querySelectOpts _ = [Asc MarketplacePlanName]

getAdminMarketplacePlansR :: Handler Value
getAdminMarketplacePlansR = do
    query <- runInputGet queryForm
    runDB $ toJSON <$> selectList (queryFilters query) (querySelectOpts query)

data BriefAccountOwner = BriefAccountOwner
    { baoName :: OwnerName
    , baoAccountId :: Maybe MarketplaceAccountId
    , baoExpired :: Bool
    , baoRepos :: Int
    }

briefAccountOwner
    :: UTCTime -> Entity MarketplaceAccount -> Int -> BriefAccountOwner
briefAccountOwner now (Entity accountId MarketplaceAccount {..}) repos =
    BriefAccountOwner
        { baoName = nameToName marketplaceAccountGithubLogin
        , baoAccountId = Just accountId
        , baoExpired = maybe False (<= now) marketplaceAccountExpiresAt
        , baoRepos = repos
        }

briefNonAccountOwner :: OwnerName -> Int -> BriefAccountOwner
briefNonAccountOwner owner repos = BriefAccountOwner
    { baoName = owner
    , baoAccountId = Nothing
    , baoExpired = False
    , baoRepos = repos
    }

getAdminMarketplacePlansNoneR :: Handler Html
getAdminMarketplacePlansNoneR = do
    accounts <- map (uncurry briefNonAccountOwner)
        <$> runDB fetchUniqueOwnersWithoutPlan

    let expiredAccounts :: [BriefAccountOwner]
        expiredAccounts = []

        marketplacePlanName :: Text
        marketplacePlanName = "No Markeplace Plan"

        marketplacePlanMonthlyRevenue :: UsCents
        marketplacePlanMonthlyRevenue = 0

    adminLayout $ do
        setTitle "Admin - Marketplace Plan"
        $(widgetFile "admin/marketplace/plan")

getAdminMarketplacePlanR :: MarketplacePlanId -> Handler Html
getAdminMarketplacePlanR planId = do
    now <- liftIO getCurrentTime
    (MarketplacePlan {..}, allAccounts) <-
        runDB
        $ (,)
        <$> get404 planId
        <*> (map (uncurry $ briefAccountOwner now)
            <$> fetchMarketplaceAccountsWithPlan planId
            )

    let (expiredAccounts, accounts) = partition baoExpired allAccounts

    adminLayout $ do
        setTitle "Admin - Marketplace Plan"
        $(widgetFile "admin/marketplace/plan")

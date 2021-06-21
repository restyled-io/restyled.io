{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Restyled.Handlers.Admin.Marketplace.Accounts
    ( getAdminMarketplaceAccountsR
    , getAdminMarketplaceAccountR
    , patchAdminMarketplaceAccountR
    ) where

import Restyled.Prelude

import Formatting (format)
import Formatting.Time (dateDash, diff, hm)
import Restyled.Foundation
import Restyled.Metrics
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.Time
import Restyled.Yesod

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

getAdminMarketplaceAccountR :: MarketplaceAccountId -> Handler Html
getAdminMarketplaceAccountR accountId = do
    now <- liftIO getCurrentTime
    (jobs, account@MarketplaceAccount {..}, mPlan) <- runDB $ do
        account <- get404 accountId
        jobs <- selectList
            [ JobOwner ==. nameToName (marketplaceAccountGithubLogin account)
            , JobCreatedAt >=. subtractTime (Days 90) now
            ]
            [Desc JobCreatedAt]
        (jobs, account, ) <$> get (marketplaceAccountMarketplacePlan account)

    let JobMetrics {..} = buildJobMetrics jobs

    adminLayout $ do
        setTitle
            $ "Admin - Marketplace - "
            <> toHtml marketplaceAccountGithubLogin
        $(widgetFile "admin/marketplace/account")

data AccountRevenue
    = RealRevenue UsCents
    | TrialRevenue UTCTime UsCents

accountRevenue
    :: UTCTime -> MarketplaceAccount -> MarketplacePlan -> AccountRevenue
accountRevenue now MarketplaceAccount {..} plan =
    case marketplaceAccountTrialEndsAt of
        Just endsAt | endsAt >= now ->
            TrialRevenue endsAt $ marketplacePlanMonthlyRevenue plan
        _ -> RealRevenue $ marketplacePlanMonthlyRevenue plan

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

{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Marketplace.Plans
    ( getAdminMarketplacePlansR
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
    }

briefAccountOwner :: UTCTime -> Entity MarketplaceAccount -> BriefAccountOwner
briefAccountOwner now (Entity accountId MarketplaceAccount {..}) =
    BriefAccountOwner
        { baoName = nameToName marketplaceAccountGithubLogin
        , baoAccountId = Just accountId
        , baoExpired = maybe False (<= now) marketplaceAccountExpiresAt
        }

getAdminMarketplacePlanR :: MarketplacePlanId -> Handler Html
getAdminMarketplacePlanR planId = do
    now <- liftIO getCurrentTime
    (plan, accounts) <-
        runDB
        $ (,)
        <$> get404 planId
        <*> (map (briefAccountOwner now)
            <$> selectList [MarketplaceAccountMarketplacePlan ==. planId] []
            )

    adminLayout $ do
        setTitle "Admin - Marketplace Plan"
        $(widgetFile "admin/marketplace/plan")

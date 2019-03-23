{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Marketplace
    ( synchronizeMarketplacePlans
    , MarketplacePlanAllows(..)
    , MarketplacePlanLimitation(..)
    , marketplacePlanAllows
    )
where


import Import hiding (runDB)

import Backend.DB
import Backend.Foundation
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Casing
import SVCS.GitHub.ApiClient

data GitHubMarketplacePlan = GitHubMarketplacePlan
    { ghmpId :: Int
    , ghmpName :: Text
    , ghmpDescription :: Text
    }
    deriving (Show, Generic)

instance FromJSON GitHubMarketplacePlan where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GitHubAccount = GitHubAccount
    { ghaId :: GitHubUserId
    , ghaLogin :: GitHubUserName
    }
    deriving (Show, Generic)

instance FromJSON GitHubAccount where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

synchronizeMarketplacePlans :: MonadBackend m => m a
synchronizeMarketplacePlans = do
    handleAny (logWarnN . tshow) runSynchronize
    liftIO $ threadDelay $ 5 * 60 * 1000000
    synchronizeMarketplacePlans

runSynchronize :: MonadBackend m => m ()
runSynchronize = do
    logInfoN "Synchronizing GitHub Marketplace data"
    plans <- getGitHub $ marketplaceListingPath <> "/plans"
    synchronizedAccountIds <- for plans $ \plan -> do
        logDebugN $ "Plan: " <> tshow plan
        planId <- runDB $ entityKey <$> upsert
            MarketplacePlan
                { marketplacePlanGithubId = ghmpId plan
                , marketplacePlanName = ghmpName plan
                , marketplacePlanDescription = ghmpDescription plan
                }
            [ MarketplacePlanName =. ghmpName plan
            , MarketplacePlanDescription =. ghmpDescription plan
            ]

        accounts <-
            getGitHub
            $ marketplaceListingPath
            <> "/plans/"
            <> toPathPiece (ghmpId plan)
            <> "/accounts"

        for accounts $ \account -> do
            logDebugN $ "Account: " <> tshow account
            runDB $ entityKey <$> upsert
                MarketplaceAccount
                    { marketplaceAccountGithubId = ghaId account
                    , marketplaceAccountGithubLogin = ghaLogin account
                    , marketplaceAccountMarketplacePlan = planId
                    }
                [MarketplaceAccountMarketplacePlan =. planId]

    logInfoN "GitHub Marketplace data synchronized"
    runDB $ deleteUnsynchronized $ mconcat synchronizedAccountIds

deleteUnsynchronized :: MonadIO m => [MarketplaceAccountId] -> SqlPersistT m ()
deleteUnsynchronized synchronizedAccountIds = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan

    deleteWhere
        [ MarketplaceAccountId /<-. synchronizedAccountIds
        , MarketplaceAccountMarketplacePlan !=. planId
        ]

fetchDiscountMarketplacePlan
    :: MonadIO m => SqlPersistT m (Entity MarketplacePlan)
fetchDiscountMarketplacePlan =
    assertJust "Discount Plan must exist"
        =<< selectFirst [MarketplacePlanGithubId ==. 0] []

getGitHub :: (FromJSON a, MonadBackend m) => Text -> m a
getGitHub path = do
    AppSettings {..} <- asks backendSettings

    liftIO $ do
        request <- parseRequest $ unpack $ "GET https://api.github.com" <> path
        requestJWT appGitHubAppId appGitHubAppKey request

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly

-- | Current, naive @'MarketplacePlan'@ limitations
marketplacePlanAllows
    :: MonadIO m => Entity Repo -> SqlPersistT m MarketplacePlanAllows
marketplacePlanAllows (Entity _ Repo {..}) = do
    mPlan <- fetchMarketplacePlanByLogin $ ownerToUserName repoOwner

    pure $ case (repoIsPrivate, mPlan) of
        (False, _) -> MarketplacePlanAllows
        (True, Nothing) -> MarketplacePlanForbids MarketplacePlanNotFound
        (True, Just plan)
            | isPrivateRepoPlan plan -> MarketplacePlanAllows
            | otherwise -> MarketplacePlanForbids MarketplacePlanPublicOnly

isPrivateRepoPlan :: MarketplacePlan -> Bool
isPrivateRepoPlan MarketplacePlan {..} =
    marketplacePlanGithubId `elem` privateRepoPlanGitHubIds

-- | Only our fake "Friends & Family" plan allows Private today
privateRepoPlanGitHubIds :: [Int]
privateRepoPlanGitHubIds = [0]

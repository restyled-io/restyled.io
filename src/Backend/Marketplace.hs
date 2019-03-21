{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Backend.Marketplace
    ( synchronizeMarketplacePlans
    )
where


import Import hiding (runDB)

import Backend.DB
import Backend.Foundation
import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Aeson.Casing
import SVCS.GitHub.JWTClient

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

marketplaceListing :: Text
--marketplaceListing = "/marketplace_listing"
marketplaceListing = "/marketplace_listing/stubbed"

synchronizeMarketplacePlans :: MonadBackend m => m a
synchronizeMarketplacePlans = do
    handleAny (logWarnN . tshow) runSynchronize
    liftIO $ threadDelay $ 5 * 60 * 1000000
    synchronizeMarketplacePlans

runSynchronize :: MonadBackend m => m ()
runSynchronize = do
    logInfoN "Synchronizing GitHub Marketplace data"
    plans <- getGitHub $ marketplaceListing <> "/plans"

    for_ @[_] plans $ \plan -> do
        logDebugN $ "Plan: " <> tshow plan

        Entity planId _ <- runDB $ upsert
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
            $ marketplaceListing
            <> "/plans/"
            <> toPathPiece (ghmpId plan)
            <> "/accounts"

        for_ @[_] accounts $ \account -> do
            logDebugN $ "Account: " <> tshow account

            runDB $ updateWhere
                [ UserGithubUserId ==. Just (ghaId account)
                , UserGithubUsername ==. Just (ghaLogin account)
                ]
                [UserMarketplacePlan =. Just planId]

    logInfoN "GitHub Marketplace data synchronized"

getGitHub :: (FromJSON a, MonadBackend m) => Text -> m a
getGitHub path = do
    AppSettings {..} <- asks backendSettings

    liftIO $ do
        request <- parseRequest $ unpack $ "GET https://api.github.com" <> path
        requestJWT appGitHubAppId appGitHubAppKey request

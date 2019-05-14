{-# LANGUAGE FlexibleContexts #-}

-- | Type and handler for a Webhook we've decided to accept
module Backend.AcceptedWebhook
    ( AcceptedWebhook(..)
    , acceptWebhook
    )
where

import Import hiding (runDB)

import Backend.DB
import Backend.Foundation
import Backend.Marketplace
import Control.Monad.Except
import Data.Aeson
import SVCS.GitHub.Webhook (GitHubPayload(..))

data AcceptedWebhook = AcceptedWebhook
    { awRepo :: Entity Repo
    , awJob :: Entity Job
    , awMarketplaceAllows :: MarketplacePlanAllows
    }

acceptWebhook
    :: MonadBackend m
    => ByteString
    -> ExceptT IgnoredWebhookReason m AcceptedWebhook
acceptWebhook body = do
    payload <-
        fmap unGitHubPayload
        $ withExceptT InvalidJSON
        $ ExceptT
        $ pure
        $ eitherDecodeStrict body

    repo <- ExceptT $ runDB $ initializeFromWebhook payload

    lift
        $ runDB
        $ AcceptedWebhook repo
        <$> insertJob repo (pPullRequest payload)
        <*> marketplacePlanAllows repo

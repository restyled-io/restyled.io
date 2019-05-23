-- | Type and handler for a Webhook we've decided to accept
module Backend.AcceptedWebhook
    ( AcceptedWebhook(..)
    , acceptWebhook
    )
where

import Backend.Import

import Backend.Marketplace

data AcceptedWebhook = AcceptedWebhook
    { awRepo :: Entity Repo
    , awJob :: Entity Job
    , awMarketplaceAllows :: MarketplacePlanAllows
    }

acceptWebhook
    :: HasDB env
    => ByteString
    -> ExceptT IgnoredWebhookReason (RIO env) AcceptedWebhook
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

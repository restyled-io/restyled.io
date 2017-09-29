{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Webhooks where

import Import

import GitHub.Webhooks.PullRequest
import Restyler

postWebhooksR :: Handler ()
postWebhooksR = do
    now <- liftIO getCurrentTime
    payload <- requireJsonBody
    $(logDebug) $ "Webhook payload received: " <> tshow payload

    webhookPayloadId <- runDB $ do
        repositoryId <- findOrCreate $ toRepository payload
        pullRequestId <- findOrCreate $ toPullRequest repositoryId payload

        insert WebhookPayload
            { webhookPayloadCreatedAt = now
            , webhookPayloadUpdatedAt = now
            , webhookPayloadState = Created
            , webhookPayloadInstallationId = pInstallationId payload
            , webhookPayloadRepository = repositoryId
            , webhookPayloadPullRequest = pullRequestId
            }

    settings <- getsYesod appSettings
    when (appProcessWebhooks settings) $ runRestyler settings webhookPayloadId

    sendResponseStatus status201 ()

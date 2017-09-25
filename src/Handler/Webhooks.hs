{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Webhooks where

import Import

import GitHub.Webhooks.PullRequest

postWebhooksR :: Handler ()
postWebhooksR = do
    now <- liftIO $ getCurrentTime
    payload <- requireJsonBody
    $(logDebug) $ "Webhook payload received: " <> tshow payload

    runDB $ do
        repositoryId <- findOrCreate $ toRepository payload
        pullRequestId <- findOrCreate $ toPullRequest repositoryId payload

        insert_ $ WebhookPayload
            { webhookPayloadCreatedAt = now
            , webhookPayloadUpdatedAt = now
            , webhookPayloadState = Created
            , webhookPayloadInstallationId = pInstallationId payload
            , webhookPayloadRepository = repositoryId
            , webhookPayloadPullRequest = pullRequestId
            }

    sendResponseStatus status201 ()

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Foundation
import Backend.Job
import Data.CaseInsensitive (CI)
import SVCS.GitHub.Webhook
import SVCS.GitLab.Webhook

postWebhooksR :: Handler ()
postWebhooksR = void $ runMaybeT $ asum
    [ handleWebhook "X-GitHub-Event" "pull_request" unGitHubPayload
    , handleWebhook "X-GitLab-Event" "Merge Request Hook" unGitLabPayload
    , rejectRequest
    ]

handleWebhook
    :: FromJSON a
    => CI ByteString -- ^ Header name for event type
    -> Text -- ^ Header value for PR event(s)
    -> (a -> Payload) -- ^ How to unwrap this SVCS' payload to @'Payload'@
    -> MaybeT Handler ()
handleWebhook header event unwrap = do
    value <- decodeUtf8 <$> MaybeT (lookupHeader header)

    if value == event
        then lift $ do
            payload <- unwrap <$> requireCheckJsonBody
            logDebugN $ "Webhook received: " <> tshow payload

            result <- runDB $ initializeFromWebhook payload
            either handleDiscarded (handleInitialized payload) result
        else do
            payload <- requireCheckJsonBody
            logDebugN $ "Webhook received: " <> tshow @Value payload
            handleDiscarded $ IgnoredEventType value

handleInitialized :: Payload -> Entity Repo -> Handler a
handleInitialized payload repo = do
    job <- runDB $ insertJob repo $ pPullRequest payload
    runBackendHandler $ enqueueRestylerJob job
    sendResponseStatus status201 ()

handleDiscarded :: MonadHandler m => IgnoredWebhookReason -> m a
handleDiscarded reason = do
    logWarnN $ "Webhook discarded: " <> reasonToLogMessage reason
    sendResponseStatus status200 ()

rejectRequest :: MonadHandler m => m a
rejectRequest = do
    logDebugN "Rejecting webhook request"
    sendResponseStatus status400 ()

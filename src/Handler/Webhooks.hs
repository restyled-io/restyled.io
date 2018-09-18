{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Foundation
import Backend.Job
import Metrics
import SVCS.GitHub.Webhook

postWebhooksR :: Handler ()
postWebhooksR = maybe rejectRequest handleGitHubEvent =<< githubEventHeader

githubEventHeader :: Handler (Maybe Text)
githubEventHeader = decodeUtf8 <$$> lookupHeader "X-GitHub-Event"

handleGitHubEvent :: Text -> Handler a
handleGitHubEvent = \case
    "pull_request" -> do
        GitHubPayload payload <- requireJsonBody
        logDebugN $ "PullRequestEvent received: " <> tshow payload
        webhookReceived

        result <- runDB $ initializeFromWebhook payload
        either handleDiscarded (handleInitialized payload) result

    "ping" -> do
        event <- requireJsonBody
        logDebugN $ "PingEvent received: " <> tshow @Value event
        handleDiscarded $ IgnoredEventType "ping"

    event -> handleDiscarded $ IgnoredEventType event

handleInitialized :: Payload -> Entity Repo -> Handler a
handleInitialized payload repo = do
    job <- runDB $ insertJob repo $ pPullRequest payload
    runBackendHandler $ enqueueRestylerJob job
    sendResponseStatus status201 ()

handleDiscarded :: MonadHandler m => IgnoredWebhookReason -> m a
handleDiscarded reason = do
    logWarnN $ "Webhook discarded: " <> reasonToLogMessage reason
    sendResponseStatus status200 ()

reasonToLogMessage :: IgnoredWebhookReason -> Text
reasonToLogMessage = \case
    IgnoredAction action -> "ignored action: " <> tshow action
    IgnoredEventType event -> "ignored event: " <> tshow event
    OwnPullRequest author -> "PR appears to be our own, author=" <> author
    PrivateNoPlan owner repo -> "private without plan: " <> repoPath owner repo

rejectRequest :: MonadHandler m => m a
rejectRequest = do
    logDebugN "Rejecting webhook request"
    sendResponseStatus status400 ()

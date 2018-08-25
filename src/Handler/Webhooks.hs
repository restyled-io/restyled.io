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
import GitHub.Data.PullRequests
import GitHub.Data.Webhooks.PullRequest
import Metrics

postWebhooksR :: Handler ()
postWebhooksR = maybe rejectRequest handleGitHubEvent =<< githubEventHeader

handleGitHubEvent :: Text -> Handler a
handleGitHubEvent = \case
    "pull_request" -> do
        payload <- requireJsonBody
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
    let prNumber = mkPullRequestNum $ pullRequestNumber $ pPullRequest payload
    job <- runDB $ insertJob repo prNumber
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
    OwnPullRequest branch -> "branch appears to be our own: " <> branch
    PrivateNoPlan owner repo ->
        "private repository with no plan: "
            <> toPathPiece owner
            <> "/"
            <> toPathPiece repo

githubEventHeader :: Handler (Maybe Text)
githubEventHeader = decodeUtf8 <$$> lookupHeader "X-GitHub-Event"

rejectRequest :: MonadHandler m => m a
rejectRequest = do
    logDebugN "Rejecting webhook request"
    sendResponseStatus status400 ()

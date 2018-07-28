{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Foundation
import Backend.Job
import GitHub.Data hiding (Repo(..))
import GitHub.Data.Webhooks.PullRequest

postWebhooksR :: Handler ()
postWebhooksR =
    maybe (sendResponseStatus status400 ()) handleGitHubEvent
        =<< githubEventHeader

githubEventHeader :: Handler (Maybe Text)
githubEventHeader = decodeUtf8 <$$> lookupHeader "X-GitHub-Event"

handleGitHubEvent :: Text -> Handler a
handleGitHubEvent = \case
    "ping" -> do
        event <- requireJsonBody
        logDebugN $ "PingEvent received: " <> tshow @Value event
        sendResponseStatus status200 ()

    "pull_request" -> do
        payload <- requireJsonBody
        logDebugN $ "PullRequestEvent received: " <> tshow payload

        result <- runDB $ initializeFromWebhook payload
        either handleDiscarded (handleInitialized payload) result

    event -> handleDiscarded $ IgnoredEventType event

handleDiscarded :: MonadHandler m => IgnoredWebhookReason -> m a
handleDiscarded reason = do
    logWarnN $ "Webhook discarded: " <> reasonToLogMessage reason
    sendResponseStatus status200 ()

handleInitialized :: Payload -> Entity Repo -> Handler a
handleInitialized payload repo = do
    let prNumber = mkId Proxy $ pullRequestNumber $ pPullRequest payload
    job <- runDB $ insertJob repo prNumber

    runBackendHandler $ enqueueRestylerJob job
    sendResponseStatus status201 ()

reasonToLogMessage :: IgnoredWebhookReason -> Text
reasonToLogMessage = \case
    IgnoredAction action -> "ignored action: " <> tshow action
    IgnoredEventType event -> "ignored event: " <> tshow event
    OwnPullRequest branch -> "branch appears to be our own: " <> branch
    PrivateNoPlan owner repo ->
        "private repository with no plan: "
            <> toPathPart owner
            <> "/"
            <> toPathPart repo

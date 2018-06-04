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

        mJob <- runDB $ do
            mRepo <- initializeFromWebhook payload

            for mRepo
                $ \repo ->
                      insertJob repo
                          $ mkId Proxy
                          $ pullRequestNumber
                          $ pPullRequest payload

        enqueueJobAndSendStatus mJob

    event -> do
        logWarnN $ "Ignored unknown GitHub event: " <> event
        sendResponseStatus status200 ()

enqueueJobAndSendStatus :: Maybe (Entity Job) -> Handler a
enqueueJobAndSendStatus (Just job) = do
    runBackendHandler $ enqueueRestylerJob job
    sendResponseStatus status201 ()
enqueueJobAndSendStatus Nothing = sendResponseStatus status200 ()

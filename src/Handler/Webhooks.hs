{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Foundation
import Backend.Job
import GitHub.Data
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

        if acceptPayload payload
            then do
                restylePullRequest payload
                sendResponseStatus status201 ()
            else sendResponseStatus status200 ()

    event -> do
        logWarnN $ "Ignored unknown GitHub event: " <> event
        sendResponseStatus status200 ()

restylePullRequest :: Payload -> Handler ()
restylePullRequest Payload {..} = do
    job <- runDB $ do
        repo <- findOrCreateRepo pRepository pInstallationId
        insertJob repo $ mkId Proxy $ pullRequestNumber pPullRequest
    runBackendHandler $ enqueueRestylerJob job

-- brittany-disable-next-binding
acceptPayload :: Payload -> Bool
acceptPayload Payload {..}
    | pAction `notElem` [PullRequestOpened, PullRequestSynchronized] = False
    -- Avoid infinite loop (best-effort)
    | "-restyled" `isSuffixOf` branchName = False
    -- Private repositories will (some day) require subscription
    | repoPrivate pRepository = False
    | otherwise = True
  where
    branchName = pullRequestCommitRef $ pullRequestHead pPullRequest

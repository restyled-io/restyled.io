{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Webhooks
    ( postWebhooksR
    ) where

import Import

import Backend.Foundation
import Backend.Job
import GitHub.Data
import GitHub.Data.Webhooks.PullRequest

postWebhooksR :: Handler ()
postWebhooksR = do
    payload@Payload{..} <- requireJsonBody
    logDebugN $ "Webhook payload received: " <> tshow payload

    if acceptPayload payload
        then do
            job <- runDB $ do
                repo <- findOrCreateRepo pRepository pInstallationId
                insertJob repo $ mkId Proxy $ pullRequestNumber pPullRequest
            runBackendHandler $ enqueueRestylerJob job
            sendResponseStatus status201 ()
        else
            -- It was valid, but we didn't create anything
            sendResponseStatus status200 ()

acceptPayload :: Payload -> Bool
acceptPayload Payload{..}
    | pAction `notElem` [Opened, Synchronize] = False

    -- Avoid infinite loop (best-effort)
    | "-restyled" `isSuffixOf` branchName = False

    -- Private repositories will (some day) require subscription
    | repoPrivate pRepository = False

    | otherwise = True

  where
    branchName = pullRequestCommitRef $ pullRequestHead pPullRequest

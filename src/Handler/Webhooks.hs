{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Webhooks where

import Import

import GitHub.Data
import GitHub.Data.Webhooks.PullRequest
import Restyler.Job

postWebhooksR :: Handler ()
postWebhooksR = do
    payload@Payload{..} <- requireJsonBody
    $(logDebug) $ "Webhook payload received: " <> tshow payload

    if acceptPayload payload
        then do
            job <- newJob pInstallationId pRepository pPullRequest
            $(logDebug) $ "Enqueuing Restyler Job: " <> tshow job
            flip enqueueRestylerJob job =<< getsYesod appRedisConn
            sendResponseStatus status201 ()
        else
            -- It was valid, but we didn't create anything
            sendResponseStatus status200 ()

acceptPayload :: Payload -> Bool
acceptPayload Payload{..}
    -- For now, we only operate when first opened
    | pAction /= Opened = False

    -- Avoid infinite loop (best-effort)
    | "-restyled" `isSuffixOf` branchName = False

    -- Always process Public
    | not $ repoPrivate pRepository = True

    -- TODO: check subscription for private repositories
    | otherwise = False

  where
    branchName = pullRequestCommitRef $ pullRequestHead pPullRequest

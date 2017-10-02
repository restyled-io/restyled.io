{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Webhooks where

import           Import

import           GitHub.Model
import           GitHub.Webhooks.PullRequest
import           Restyler

postWebhooksR :: Handler ()
postWebhooksR = do
    payload <- requireJsonBody
    $(logDebug) $ "Webhook payload received: " <> tshow payload

    if acceptPayload payload
        then do
            settings <- getsYesod appSettings
            when (appProcessWebhooks settings) $ runRestyler settings payload
            sendResponseStatus status201 ()
        else
            -- It was valid, but we didn't create anything
            sendResponseStatus status200 ()

acceptPayload :: Payload -> Bool
acceptPayload Payload{..} =
    pAction == Opened && not ("-restyled" `isSuffixOf` branchName)
  where
    branchName = unBranch $ rrRef $ prHead pPullRequest

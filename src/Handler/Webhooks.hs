{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Webhooks where

import Import

import GitHub.Webhooks.PullRequest
import Restyler

postWebhooksR :: Handler ()
postWebhooksR = do
    payload <- requireJsonBody
    $(logDebug) $ "Webhook payload received: " <> tshow payload

    if pAction payload == Opened
        then do
            settings <- getsYesod appSettings
            when (appProcessWebhooks settings) $ runRestyler settings payload
            sendResponseStatus status201 ()
        else
            -- It was valid, but we didnt' create anything.
            sendResponseStatus status200 ()

module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Webhook
import Data.Conduit.Binary

postWebhooksR :: Handler ()
postWebhooksR = do
    body <- runConduit $ rawRequestBody .| sinkLbs
    runHandlerRIO $ enqueueWebhook $ toStrict body
    sendResponseStatus @_ @Text status201 ""

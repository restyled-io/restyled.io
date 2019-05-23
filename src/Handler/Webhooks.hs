module Handler.Webhooks
    ( postWebhooksR
    )
where

import Import

import Backend.Webhook
import Conduit
import Data.ByteString.Lazy (toStrict)
import Data.Conduit.Binary
import Foundation
import Network.HTTP.Types.Status
import Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
    body <- runConduit $ rawRequestBody .| sinkLbs
    runHandlerRIO $ enqueueWebhook $ toStrict body
    sendResponseStatus @_ @Text status201 ""

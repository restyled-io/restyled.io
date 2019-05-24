module Restyled.Handlers.Webhooks
    ( postWebhooksR
    )
where

import Restyled.Prelude

import Conduit
import Data.ByteString.Lazy (toStrict)
import Data.Conduit.Binary
import Restyled.Backend.Webhook
import Restyled.Foundation
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
    body <- runConduit $ rawRequestBody .| sinkLbs
    runRedis $ enqueueWebhook $ toStrict body
    sendResponseStatus @_ @Text status201 ""

module Restyled.Handlers.Webhooks
    ( postWebhooksR
    ) where

import Restyled.Prelude

import Conduit
import Data.ByteString.Lazy (toStrict)
import Data.Conduit.Binary
import Restyled.Foundation
import Restyled.Queues
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
    qs <- view queuesL
    body <- runConduit $ rawRequestBody .| sinkLbs
    runRedis $ enqueue qs $ toStrict body
    sendResponseStatus @_ @Text status201 ""

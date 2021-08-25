module Restyled.Handlers.Webhooks
    ( postWebhooksR
    ) where

import Restyled.Prelude

import Conduit
import Data.ByteString.Lazy (toStrict)
import Data.Conduit.Binary
import Restyled.Foundation
import Restyled.Settings
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
    body <- runConduit $ rawRequestBody .| sinkLbs
    queue <- appRestylerQueue <$> view settingsL
    void $ runRedis $ lpush queue [toStrict body]
    sendResponseStatus @_ @Text status201 ""

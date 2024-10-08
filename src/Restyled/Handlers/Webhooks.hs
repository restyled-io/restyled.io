module Restyled.Handlers.Webhooks
  ( postWebhooksR
  ) where

import Restyled.Prelude

import Conduit
import Data.Conduit.Binary
import Restyled.Disabled
import Restyled.Foundation
import Restyled.Queues
import Restyled.Redis
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
  qs <- view queuesL
  body <- runConduit $ rawRequestBody .| sinkLbs

  emitDisabledStatus body >>= \case
    Left err -> do
      logDebug $ "Disabled status not emitted" :# ["reason" .= err]
      runRedis $ enqueue qs $ toStrict body -- proceed normally
    Right status -> do
      logInfo $ "Disabled status emitted" :# ["status" .= show @Text status]

  sendResponseStatus @_ @Text status201 ""

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
  body <- runConduit $ rawRequestBody .| sinkLbs

  emitDisabledStatus body >>= \case
    Left {} -> do
      -- Not emitted, proceed normally
      qs <- view queuesL
      runRedis $ enqueue qs $ toStrict body
    Right {} -> pure ()

  sendResponseStatus @_ @Text status201 ""

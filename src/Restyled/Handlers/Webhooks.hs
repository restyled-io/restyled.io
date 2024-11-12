module Restyled.Handlers.Webhooks
  ( postWebhooksR
  ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = sendResponseStatus @_ @Text status200 ""

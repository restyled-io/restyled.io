module Restyled.Handlers.Webhooks
    ( postWebhooksR
    ) where

import Restyled.Prelude

import Conduit
import Data.Aeson.Lens
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit.Binary
import Restyled.Backend.Webhook
import Restyled.Foundation
import Restyled.Yesod

postWebhooksR :: Handler ()
postWebhooksR = do
    body <- runConduit $ rawRequestBody .| sinkLbs
    runRedis $ if enqueueToExperimentalRestyler body
        then enqueueWebhookTo experimentalQueueName $ toStrict body
        else enqueueWebhook $ toStrict body
    sendResponseStatus @_ @Text status201 ""

enqueueToExperimentalRestyler :: BSL.ByteString -> Bool
enqueueToExperimentalRestyler body = fromMaybe False $ do
    name <- body ^? key "repository" . key "full_name" . _String
    pure $ name `elem` experimentalRepositories

experimentalRepositories :: [Text]
experimentalRepositories =
    ["restyled-io/demo", "restyled-io/restyled.io", "restyled-io/agent"]

experimentalQueueName :: ByteString
experimentalQueueName = "restyled:agent:webhooks"

module Restyled.Backend.Application
    ( runWebhooks
    ) where

import Restyled.Prelude

import Restyled.Backend.DockerRunJob
import Restyled.Backend.ExecRestyler
import Restyled.Backend.Webhook
import Restyled.Settings

runWebhooks
    :: ( HasLogFunc env
       , HasSettings env
       , HasSqlPool env
       , HasRedis env
       , HasProcessContext env
       )
    => RIO env a
runWebhooks = forever $ do
    mWebhook <- awaitWebhook 120
    traverse_ (processWebhook $ ExecRestyler dockerRunJob) mWebhook

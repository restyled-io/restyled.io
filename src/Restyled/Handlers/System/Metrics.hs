module Restyled.Handlers.System.Metrics
    ( getSystemMetricsR
    ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Metric
import Restyled.Queues
import Restyled.Yesod

newtype Metrics n = Metrics
    { metrics :: [Metric n]
    }
    deriving stock Generic
    deriving anyclass ToJSON

getSystemMetricsR :: Handler Value
getSystemMetricsR = do
    qs <- view queuesL
    metrics <- Metrics <$> getQueuesMetrics qs
    sendStatusJSON status200 metrics

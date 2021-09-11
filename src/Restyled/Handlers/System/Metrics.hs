module Restyled.Handlers.System.Metrics
    ( getSystemMetricsR
    ) where

import Restyled.Prelude

import qualified Restyled.Backend.Webhook as Webhook
import Restyled.Foundation
import Restyled.Time
import Restyled.TimeRange
import Restyled.Yesod

data Metric n = Metric
    { mName :: Text
    , mValue :: n
    , mUnit :: Unit
    }

metric :: Text -> n -> Metric n
metric = metricUnit Count

metricUnit :: Unit -> Text -> n -> Metric n
metricUnit unit name value =
    Metric { mName = name, mValue = value, mUnit = unit }

instance ToJSON n => ToJSON (Metric n) where
    toJSON Metric {..} =
        object ["MetricName" .= mName, "Value" .= mValue, "Unit" .= mUnit]

data Unit = Count | Percent

instance ToJSON Unit where
    toJSON Count = String "Count"
    toJSON Percent = String "Percent"

getSystemMetricsR :: Handler Value
getSystemMetricsR = do
    depth <- runRedis Webhook.queueDepth
    mMins <- runInputGet $ iopt intField "since-minutes"
    range <- timeRangeFromAgo $ Minutes $ min (6 * 60) $ fromMaybe 5 mMins
    pure $ object ["range" .= range, "metrics" .= [metric "QueueDepth" depth]]

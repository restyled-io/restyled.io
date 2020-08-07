module Restyled.Handlers.System.Metrics
    ( getSystemMetricsR
    )
where

import Restyled.Prelude

import Data.Semigroup (Sum(..))
import qualified Restyled.Backend.Webhook as Webhook
import Restyled.Foundation
import Restyled.Metrics
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
    range <- timeRangeFromMinutesAgo $ min (6 * 60) $ fromMaybe 5 mMins
    JobMetrics {..} <- runDB $ fetchJobMetrics range

    let succeeded = getSum jmSucceeded
        failed = getSum jmFailed
        completed = succeeded + failed

        successPercent :: Double
        successPercent
            | completed == 0
            = 0
            | otherwise
            = (* 100) $ fromIntegral succeeded / fromIntegral completed

    pure $ object
        [ "range" .= range
        , "metrics" .= toJSON
            [ toJSON $ metric "QueueDepth" depth
            , toJSON $ metric "JobsSucceeded" succeeded
            , toJSON $ metric "JobsFailed" failed
            , toJSON $ metricUnit Percent "JobsSuccessRate" successPercent
            ]
        ]

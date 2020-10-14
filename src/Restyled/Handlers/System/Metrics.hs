module Restyled.Handlers.System.Metrics
    ( getSystemMetricsR
    )
where

import Restyled.Prelude

import Data.Semigroup (Sum(..))
import qualified Restyled.Backend.Webhook as Webhook
import Restyled.Foundation
import Restyled.Metrics
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
    JobMetrics {..} <- runDB $ fetchJobMetrics range

    let succeeded = getSum jmSucceeded
        failed = getSum jmFailed
        failedUnknown = getSum jmFailedUnknown
        completed = succeeded + failed
        unfinished = getSum jmUnfinished

        successPercent :: Maybe Double
        successPercent
            | completed == 0
            = Nothing
            | otherwise
            = Just $ (* 100) $ fromIntegral succeeded / fromIntegral completed

        metrics :: [Value]
        metrics = catMaybes
            [ Just $ toJSON $ metric "QueueDepth" depth
            , Just $ toJSON $ metric "JobsSucceeded" succeeded
            , Just $ toJSON $ metric "JobsFailed" failed
            , Just $ toJSON $ metric "JobsFailedUnknown" failedUnknown
            , Just $ toJSON $ metric "JobsUnfinished" unfinished
            , toJSON . metricUnit Percent "JobsSuccessRate" <$> successPercent
            ]

    pure $ object ["range" .= range, "metrics" .= metrics]

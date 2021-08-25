module Restyled.Handlers.System.Metrics
    ( getSystemMetricsR
    ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Metrics
import Restyled.Settings
import Restyled.SqlError
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
    queue <- appRestylerQueue <$> view settingsL
    depth <- runRedis $ hush <$> llen queue
    mMins <- runInputGet $ iopt intField "since-minutes"
    range <- timeRangeFromAgo $ Minutes $ min (6 * 60) $ fromMaybe 5 mMins
    JobMetrics {..} <-
        handleSqlError (\ex -> mempty <$ logJobMetricsError ex)
        $ runDB
        $ fetchJobMetrics range

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

-- We're having an issue where our Postgres starts timing out transactions to
-- our Heroku app. We don't know why, but an inability to get metrics leads to
-- alarms, and a lack of knowing depth (which we can know without DB data)
-- prevents scaling.
logJobMetricsError :: MonadLogger m => SqlError -> m ()
logJobMetricsError = logErrorN . ("[SystemMetricsR]: " <>) . displaySqlError

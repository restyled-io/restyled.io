module Restyled.Handlers.Admin.Metrics
    ( getAdminMetricsR
    )
where

import Restyled.Prelude hiding (to)

import Restyled.Foundation
import Restyled.Metrics
import Restyled.TimeRange
import Restyled.Yesod

getAdminMetricsR :: Handler Value
getAdminMetricsR = do
    range <- requiredTimeRange
    metrics <- runDB $ fetchJobMetricsByHour range
    sendResponse $ toJSON metrics

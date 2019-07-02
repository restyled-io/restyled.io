module Restyled.Handlers.Admin.Metrics
    ( getAdminMetricsR
    )
where

import Restyled.Prelude hiding (to)

import Restyled.Foundation
import Restyled.Metrics
import Restyled.Yesod

getAdminMetricsR :: Handler Value
getAdminMetricsR = do
    (from, to) <- runInputGet $ (,) <$> ireq epochField "from" <*> ireq
        epochField
        "to"

    metrics <- runDB $ fetchJobMetricsByHour from to
    sendResponse $ toJSON metrics

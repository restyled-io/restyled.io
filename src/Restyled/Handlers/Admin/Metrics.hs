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

epochField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m UTCTime
epochField = inputField parseEpoch

parseEpoch :: Text -> Either FormMessage UTCTime
parseEpoch t =
    note errMessage $ parseTimeM True defaultTimeLocale "%s" $ unpack t
  where
    errMessage :: FormMessage
    errMessage =
        MsgInvalidNumber
            $ t
            <> " did not parse as whole seconds since Unix epoch"

inputField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
    => (Text -> Either FormMessage a)
    -> Field m a
inputField p = Field
    { fieldParse = parseHelper p
    , fieldView = error "No view for inputs"
    , fieldEnctype = error "No enctype for inputs"
    }

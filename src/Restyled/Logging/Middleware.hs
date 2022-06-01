module Restyled.Logging.Middleware
    ( requestLogger
    ) where

import Restyled.Prelude

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types.Header (Header, HeaderName)
import Network.HTTP.Types.Status (Status(..))
import Network.Wai
    ( Middleware
    , Request
    , Response
    , rawPathInfo
    , rawQueryString
    , requestHeaders
    , requestMethod
    , responseHeaders
    , responseStatus
    )
import Restyled.Settings

requestLogger :: HasSettings env => env -> Middleware
requestLogger env app req respond = do
    runAppLogging env $ do
        logRequest req

        withRunInIO $ \runInIO -> do
            app req $ \resp -> do
                recvd <- respond resp
                recvd <$ runInIO (logResponse resp)

logRequest :: MonadLogger m => Request -> m ()
logRequest req = logDebug $ "Request" :# requestDetails req

requestDetails :: Request -> [Series]
requestDetails req =
    [ "method" .= decodeUtf8 (requestMethod req)
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "query" .= decodeUtf8 (rawQueryString req)
    , "headers" .= headerObject ["authorization", "cookie"] (requestHeaders req)
    ]

logResponse :: MonadLogger m => Response -> m ()
logResponse resp
    | status >= 500 = logError $ "Response" :# responseDetails resp
    | status == 404 = logDebug $ "Response" :# responseDetails resp
    | status >= 400 = logWarn $ "Response" :# responseDetails resp
    | otherwise = logDebug $ "Response" :# responseDetails resp
    where status = statusCode $ responseStatus resp

responseDetails :: Response -> [Series]
responseDetails resp =
    [ "status" .= object
        [ "code" .= statusCode status
        , "message" .= decodeUtf8 (statusMessage status)
        ]
    , "headers" .= headerObject ["set-cookie"] (responseHeaders resp)
    ]
    where status = responseStatus resp

headerObject :: [HeaderName] -> [Header] -> Value
headerObject redact = Object . KeyMap.fromList . map (mung . hide)
  where
    mung = Key.fromText . decodeUtf8 . CI.foldedCase *** String . decodeUtf8
    hide (k, v)
        | k `elem` redact = (k, "***")
        | otherwise = (k, v)

module Logging.Terminal
    ( reformatTerminal
    ) where

import Prelude

import Control.Monad.Logger.Aeson
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime)
import Logging.Colors

reformatTerminal :: Bool -> LogLevel -> ByteString -> ByteString
reformatTerminal useColor logLevel bytes = fromMaybe bytes $ do
    LoggedMessage {..} <- decode $ BSL.fromStrict bytes

    let colors@Colors {..} = getColors useColor

        logLevelText = case logLevel of
            LevelDebug -> lightgray $ minimumWidth 9 "debug"
            LevelInfo -> green $ minimumWidth 9 "info"
            LevelWarn -> yellow $ minimumWidth 9 "warn"
            LevelError -> red $ minimumWidth 9 "error"
            LevelOther x -> blue $ minimumWidth 9 x

    pure $ encodeUtf8 $ mconcat
        [ black $ pack $ formatTime
            defaultTimeLocale
            "%F %X"
            loggedMessageTimestamp
        , " "
        , black "[" <> logLevelText <> black "]"
        , " "
        , bold $ minimumWidth 31 loggedMessageText
        , colorizeKeyMap colors $ maybe
            KeyMap.empty
            (KeyMap.singleton "source" . String)
            loggedMessageLogSource
        , colorizeKeyMap colors loggedMessageThreadContext
        , colorizeKeyMap colors loggedMessageMeta
        ]

colorizeKeyMap :: Colors -> KeyMap Value -> Text
colorizeKeyMap Colors {..} km
    | KeyMap.null km = ""
    | otherwise = " " <> T.intercalate " " keyValues
  where
    keyValues = map (uncurry fromKeyValue) $ KeyMap.toList km

    fromKeyValue k v = cyan (Key.toText k) <> "=" <> magenta (fromValue v)

    fromValue :: Value -> Text
    fromValue = \case
        Object _ -> "{...}"
        Array _ -> "[...]"
        String x -> x
        Number n -> pack $ show n
        Bool b -> pack $ show b
        Null -> "null"

minimumWidth :: Int -> Text -> Text
minimumWidth n t = t <> T.replicate pad " " where pad = max 0 $ n - T.length t

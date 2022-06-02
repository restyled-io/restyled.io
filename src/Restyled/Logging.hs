-- | TODO: make this domain-agnostic
module Restyled.Logging
    ( LogSettings
    , defaultLogSettings
    , setLogSettingsLevel
    , Logger
    , loggerLoggerSet
    , HasLogger(..)
    , newLogger
    , runAppLoggingT
    ) where

import Prelude

import Control.Lens (Lens', to, (^.))
import Control.Monad.IO.Class (MonadIO(..))
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
import Restyled.Logging.Colors
import System.IO (hIsTerminalDevice, stderr, stdout)
import System.Log.FastLogger
    ( LoggerSet
    , defaultBufSize
    , flushLogStr
    , newFileLoggerSet
    , newStderrLoggerSet
    , newStdoutLoggerSet
    , pushLogStrLn
    )

data LogSettings = LogSettings
    { lsLevel :: LogLevel
    , lsDestination :: LogDestination
    , lsFormat :: LogFormat
    , lsColor :: LogColor
    }

data LogDestination
    = LogDestinationStdout
    | LogDestinationStderr
    | LogDestinationFile FilePath

data LogFormat
    = LogFormatJSON
    | LogFormatTerminal

data LogColor
    = LogColorAuto
    | LogColorAlways
    | LogColorNever

getLogColor :: Applicative m => LogColor -> m Bool -> m Bool
getLogColor lc f = case lc of
    LogColorAuto -> f
    LogColorAlways -> pure True
    LogColorNever -> pure False

defaultLogSettings :: LogSettings
defaultLogSettings = LogSettings
    { lsLevel = LevelInfo

    -- TODO
    , lsDestination = LogDestinationStdout
    , lsFormat = LogFormatTerminal
    , lsColor = LogColorAlways
    }

setLogSettingsLevel :: LogLevel -> LogSettings -> LogSettings
setLogSettingsLevel level ls = ls { lsLevel = level }

data Logger = Logger
    { lLoggerSet :: LoggerSet
    , lLogReformat :: LogLevel -> ByteString -> ByteString
    , lMinLogLevel :: LogLevel
    }

loggerLoggerSet :: Logger -> LoggerSet
loggerLoggerSet = lLoggerSet

class HasLogger env where
    loggerL :: Lens' env Logger

instance HasLogger Logger where
    loggerL = id

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger LogSettings {..} = do
    (lLoggerSet, useColor) <- liftIO $ case lsDestination of
        LogDestinationStdout ->
            (,) <$> newStdoutLoggerSet defaultBufSize <*> getLogColor
                lsColor
                (hIsTerminalDevice stdout)
        LogDestinationStderr ->
            (,) <$> newStderrLoggerSet defaultBufSize <*> getLogColor
                lsColor
                (hIsTerminalDevice stderr)
        LogDestinationFile path ->
            (,) <$> newFileLoggerSet defaultBufSize path <*> getLogColor
                lsColor
                (hIsTerminalDevice stdout)

    let lLogReformat = case lsFormat of
            LogFormatJSON -> const id -- Color is ignored
            LogFormatTerminal -> reformatTerminal useColor

        lMinLogLevel = lsLevel

    pure $ Logger { .. }

runAppLoggingT :: (MonadIO m, HasLogger env) => env -> LoggingT m a -> m a
runAppLoggingT env f = do
    a <- flip runLoggingT (fastLoggerOutput' ls rf)
        $ filterLogger (const (>= ml)) f
    a <$ liftIO (flushLogStr ls)
  where
    ls = env ^. loggerL . to lLoggerSet
    rf = env ^. loggerL . to lLogReformat
    ml = env ^. loggerL . to lMinLogLevel

fastLoggerOutput'
    :: LoggerSet
    -> (LogLevel -> ByteString -> ByteString)
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> IO ()
fastLoggerOutput' loggerSet reformat =
    defaultOutputWith $ defaultOutputOptions $ \logLevel bytes -> do
        pushLogStrLn loggerSet $ toLogStr $ reformat logLevel bytes

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

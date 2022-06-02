module Logging.Logger
    ( Logger
    , HasLogger(..)
    , newLogger
    , getLoggerLoggerSet
    , getLoggerReformat
    , getLoggerShouldLog
    ) where

import Prelude

import Control.Lens (Lens')
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import Data.ByteString (ByteString)
import Logging.Settings
import Logging.Terminal
import System.IO (stderr, stdout)
import System.Log.FastLogger
    ( LoggerSet
    , defaultBufSize
    , newFileLoggerSet
    , newStderrLoggerSet
    , newStdoutLoggerSet
    )

data Logger = Logger
    { lLoggerSet :: LoggerSet
    , lReformat :: LogLevel -> ByteString -> ByteString
    , lShouldLog :: LogSource -> LogLevel -> Bool
    }

getLoggerLoggerSet :: Logger -> LoggerSet
getLoggerLoggerSet = lLoggerSet

getLoggerReformat :: Logger -> LogLevel -> ByteString -> ByteString
getLoggerReformat = lReformat

getLoggerShouldLog :: Logger -> LogSource -> LogLevel -> Bool
getLoggerShouldLog = lShouldLog

class HasLogger env where
    loggerL :: Lens' env Logger

instance HasLogger Logger where
    loggerL = id

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger settings = do
    (lLoggerSet, useColor) <-
        liftIO $ case getLogSettingsDestination settings of
            LogDestinationStdout ->
                (,)
                    <$> newStdoutLoggerSet defaultBufSize
                    <*> shouldColorHandle settings stdout
            LogDestinationStderr ->
                (,)
                    <$> newStderrLoggerSet defaultBufSize
                    <*> shouldColorHandle settings stderr
            LogDestinationFile path ->
                (,) <$> newFileLoggerSet defaultBufSize path <*> shouldColorAuto
                    settings
                    (pure False)

    let lReformat = case getLogSettingsFormat settings of
            LogFormatJSON -> const id -- Color is ignored
            LogFormatTerminal -> reformatTerminal useColor

        lShouldLog = shouldLogLevel settings

    pure $ Logger { .. }

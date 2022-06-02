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

import Restyled.Prelude

import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import System.Log.FastLogger.LoggerSet (LoggerSet)

newtype LogSettings = LogSettings
    { lsLevel :: LogLevel
    -- TODO: destination, format
    }

defaultLogSettings :: LogSettings
defaultLogSettings = LogSettings { lsLevel = LevelInfo }

setLogSettingsLevel :: LogLevel -> LogSettings -> LogSettings
setLogSettingsLevel level ls = ls { lsLevel = level }

data Logger = Logger
    { lLoggerSet :: LoggerSet
    , lSettings :: LogSettings
    }

loggerLoggerSet :: Logger -> LoggerSet
loggerLoggerSet = lLoggerSet

class HasLogger env where
    loggerL :: Lens' env Logger

instance HasLogger Logger where
    loggerL = id

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger ls =
    Logger <$> liftIO (newStdoutLoggerSet defaultBufSize) <*> pure ls

runAppLoggingT :: HasLogger env => env -> LoggingT m a -> m a
runAppLoggingT env = runFastLoggingT ls . filterLogger (const (>= ml))
  where
    ls = env ^. loggerL . to lLoggerSet
    ml = env ^. loggerL . to (lsLevel . lSettings)

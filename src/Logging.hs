module Logging
    ( LogSettings
    , LogLevel(..)
    , LogDestination(..)
    , LogFormat(..)
    , LogColor(..)
    , defaultLogSettings
    , setLogSettingsLevel
    , setLogSettingsDestination
    , setLogSettingsFormat
    , setLogSettingsColor
    , Logger
    , HasLogger(..)
    , newLogger
    , runLoggerLoggingT

    -- * Re-exports from "Control.Monad.Logger.Aeson"
    , Message(..)
    , Series
    , withThreadContext
    , MonadLogger(..)
    , LoggingT

    -- ** Common logging functions
    -- | Import "Control.Monad.Logger.Aeson" if you want more
    , logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    ) where

import Prelude

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import Data.Aeson (Series)
import Data.ByteString (ByteString)
import Logging.Logger
import Logging.Settings
import System.Log.FastLogger (LoggerSet, flushLogStr, pushLogStrLn)

runLoggerLoggingT :: (MonadIO m, HasLogger env) => env -> LoggingT m a -> m a
runLoggerLoggingT env f = do
    a <- runLoggingT
        (filterLogger (getLoggerShouldLog logger) f)
        (loggerOutput loggerSet $ getLoggerReformat logger)
    a <$ liftIO (flushLogStr loggerSet)
  where
    logger = env ^. loggerL
    loggerSet = getLoggerLoggerSet logger

loggerOutput
    :: LoggerSet
    -> (LogLevel -> ByteString -> ByteString)
    -> Loc
    -> LogSource
    -> LogLevel
    -> LogStr
    -> IO ()
loggerOutput loggerSet reformat =
    defaultOutputWith $ defaultOutputOptions $ \logLevel bytes -> do
        pushLogStrLn loggerSet $ toLogStr $ reformat logLevel bytes

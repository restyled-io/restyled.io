module Logging.Settings
    ( LogSettings
    , LogLevel(..)
    , LogDestination(..)
    , LogFormat(..)
    , LogColor(..)

    -- * Construction
    , defaultLogSettings

    -- * Modify
    , setLogSettingsLevel
    , setLogSettingsDestination
    , setLogSettingsFormat
    , setLogSettingsColor

    -- * Access
    , getLogSettingsLevel
    , getLogSettingsDestination
    , getLogSettingsFormat
    , getLogSettingsColor

    -- * Logic
    , shouldLogLevel
    , shouldColorAuto
    , shouldColorHandle
    ) where

import Prelude

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger.Aeson
import System.IO (Handle, hIsTerminalDevice)

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


defaultLogSettings :: LogSettings
defaultLogSettings = LogSettings
    { lsLevel = LevelInfo

    -- TODO
    , lsDestination = LogDestinationStdout
    , lsFormat = LogFormatTerminal
    , lsColor = LogColorAlways
    }

setLogSettingsLevel :: LogLevel -> LogSettings -> LogSettings
setLogSettingsLevel x ls = ls { lsLevel = x }

setLogSettingsDestination :: LogDestination -> LogSettings -> LogSettings
setLogSettingsDestination x ls = ls { lsDestination = x }

setLogSettingsFormat :: LogFormat -> LogSettings -> LogSettings
setLogSettingsFormat x ls = ls { lsFormat = x }

setLogSettingsColor :: LogColor -> LogSettings -> LogSettings
setLogSettingsColor x ls = ls { lsColor = x }

getLogSettingsLevel :: LogSettings -> LogLevel
getLogSettingsLevel = lsLevel

getLogSettingsDestination :: LogSettings -> LogDestination
getLogSettingsDestination = lsDestination

getLogSettingsFormat :: LogSettings -> LogFormat
getLogSettingsFormat = lsFormat

getLogSettingsColor :: LogSettings -> LogColor
getLogSettingsColor = lsColor

shouldLogLevel :: LogSettings -> LogSource -> LogLevel -> Bool
shouldLogLevel LogSettings {..} = const (>= lsLevel)

shouldColorAuto :: Applicative m => LogSettings -> m Bool -> m Bool
shouldColorAuto LogSettings {..} f = case lsColor of
    LogColorAuto -> f
    LogColorAlways -> pure True
    LogColorNever -> pure False

shouldColorHandle :: MonadIO m => LogSettings -> Handle -> m Bool
shouldColorHandle settings h =
    shouldColorAuto settings $ liftIO $ hIsTerminalDevice h

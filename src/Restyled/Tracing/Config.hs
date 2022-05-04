module Restyled.Tracing.Config
    ( TracingConfig(..)
    , DaemonSocket(..)
    , AppName(..)
    , LicenseKey(..)
    , TimeoutMs(..)
    , TracingLogLevel(..)
    , readTracingLogLevel
    , defaultTracingLogLevel
    ) where

import RIO hiding (LogLevel(..))

import Data.Char (toLower)
import Tracing.NewRelic

data TracingConfig = TracingConfig
    { tcDaemonSocket :: DaemonSocket
    , tcAppName :: AppName
    , tcLicenseKey :: Maybe LicenseKey
    -- ^ 'Nothing' to disable tracing
    --
    -- Other non-'Maybe' fields can use defaults in the disabled case.
    --
    , tcTimeoutMs :: TimeoutMs
    , tcLog :: Text
    -- ^ Path, or the string "stdout" or "stderr"
    , tcLogLevel :: TracingLogLevel
    }

newtype TracingLogLevel = TracingLogLevel
    { unTracingLogLevel :: LogLevel
    }

readTracingLogLevel :: String -> Either String TracingLogLevel
readTracingLogLevel = fmap TracingLogLevel . go . map toLower
  where
    go :: String -> Either String LogLevel
    go = \case
        "error" -> Right Error
        "warning" -> Right Warning
        "info" -> Right Info
        "debug" -> Right Debug
        x -> Left $ "Invalid TracingLogLevel: " <> show x

defaultTracingLogLevel :: TracingLogLevel
defaultTracingLogLevel = TracingLogLevel Info

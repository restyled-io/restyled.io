module Restyled.Logging
    ( LogSettings
    , defaultLogSettings
    , setLogSettingsLevel
    , Logger
    , toYesodLogger
    , HasLogger(..)
    , newLogger
    , runAppLoggingT
    ) where

import Restyled.Prelude

import qualified Yesod.Core as Yesod (defaultMakeLogger)
import qualified Yesod.Core.Types as Yesod (Logger(..))

newtype LogSettings = LogSettings
    { lsLevel :: LogLevel
    -- TODO: destination, format
    }

defaultLogSettings :: LogSettings
defaultLogSettings = LogSettings { lsLevel = LevelInfo }

setLogSettingsLevel :: LogLevel -> LogSettings -> LogSettings
setLogSettingsLevel level ls = ls { lsLevel = level }

data Logger = Logger
    { lLogger :: Yesod.Logger
    , lSettings :: LogSettings
    }

toYesodLogger :: Logger -> Yesod.Logger
toYesodLogger = lLogger

class HasLogger env where
    loggerL :: Lens' env Logger

instance HasLogger Logger where
    loggerL = id

newLogger :: MonadIO m => LogSettings -> m Logger
newLogger ls = Logger <$> liftIO Yesod.defaultMakeLogger <*> pure ls

runAppLoggingT :: HasLogger env => env -> LoggingT m a -> m a
runAppLoggingT env = runFastLoggingT ls . filterLogger (const (>= ml))
  where
    ls = env ^. loggerL . to (Yesod.loggerSet . lLogger)
    ml = env ^. loggerL . to (lsLevel . lSettings)

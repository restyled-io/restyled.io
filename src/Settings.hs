{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod hiding (throw)
import Data.Aeson (withObject)
import Database.Persist.Postgresql (PostgresConf(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.PGDatabaseURL (parsePGConnectionString)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Util
#if DEVELOPMENT
    (widgetFileReload)
#else
    (widgetFileNoReload)
#endif

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8

data AppSettings = AppSettings
    { appDatabaseConf :: PostgresConf
    , appRoot :: Text
    , appHost :: HostPreference
    , appPort :: Int
    , appIpFromHeader :: Bool
    , appLogLevel :: LogLevel
    , appMutableStatic :: Bool
    , appCopyright :: Text
    }

instance Show AppSettings where
    show AppSettings{..} = concat
        [ "log_level=", show appLogLevel
        , " host=", show appHost
        , " port=", show appPort
        , " root=", show appRoot
        , " db=[", C8.unpack $ pgConnStr appDatabaseConf, "]"
        ]

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        url <- o .: "database-url"
        connStr <- either fail return $ parsePGConnectionString url

        appDatabaseConf <- PostgresConf
            <$> pure connStr
            <*> o .: "database-pool-size"
        appRoot <- o .: "approot"
        appHost <- fromString <$> o .: "host"
        appPort <- o .: "port"
        appIpFromHeader <- o .: "ip-from-header"
        appLogLevel <- parseLogLevel <$> o .: "log-level"
        appMutableStatic <- o .: "mutable-static"
        appCopyright <- o .: "copyright"


        return AppSettings{..}

      where
        parseLogLevel :: Text -> LogLevel
        parseLogLevel t = case T.toLower t of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther t

-- This value is needed in a pure context, and so can't read from ENV. It also
-- doesn't differ between environments, so we might as well harcode it.
appStaticDir :: FilePath
appStaticDir = "static"

allowsLevel :: AppSettings -> LogLevel -> Bool
allowsLevel AppSettings{..} = (>= appLogLevel)

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

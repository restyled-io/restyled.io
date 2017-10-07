{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Settings where

import ClassyPrelude.Yesod hiding (Proxy, throw)

import Data.Proxy
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..))
import GitHub.Data
import GitHub.Data.Apps
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.PGDatabaseURL (parsePGConnectionString)
import Network.RedisURL (parseRedisURL)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Env

data AppSettings = AppSettings
    { appDatabaseConf :: PostgresConf
    , appRedisConf :: ConnectInfo
    , appRoot :: Text
    , appHost :: HostPreference
    , appPort :: Int
    , appIpFromHeader :: Bool
    , appLogLevel :: LogLevel
    , appMutableStatic :: Bool
    , appCopyright :: Text
    , appGitHubAppId :: Id App
    , appGitHubAppKey :: Text
    , appRestylerImage :: String
    , appRestylerTag :: Maybe String
    }

instance Show AppSettings where
    show AppSettings{..} = concat
        [ "log_level=", show appLogLevel
        , " host=", show appHost
        , " port=", show appPort
        , " root=", show appRoot
        , " db=[", C8.unpack $ pgConnStr appDatabaseConf, "]"
        ]

type EnvParser a = forall e.
    (Env.AsUnset e, Env.AsUnread e, Env.AsEmpty e) => Env.Parser e a

loadEnvSettings :: IO AppSettings
loadEnvSettings = Env.parse id envSettings

envSettings :: EnvParser AppSettings
envSettings = AppSettings
    <$> envDatabaseConfig
    <*> envRedisConfig
    <*> Env.var Env.str "APPROOT" (Env.def "http://localhost:3000")
    <*> Env.var Env.str "HOST" (Env.def "*4")
    <*> Env.var Env.auto "PORT" (Env.def 3000)
    <*> Env.switch "IP_FROM_HEADER" mempty
    <*> envLogLevel
    <*> Env.switch "MUTABLE_STATIC" mempty
    <*> pure "Patrick Brisbin 2017"
    <*> (mkId Proxy <$> Env.var Env.auto "GITHUB_APP_ID" mempty)
    <*> Env.var Env.nonempty "GITHUB_APP_KEY" mempty
    <*> Env.var Env.str "RESTYLER_IMAGE" (Env.def "restyled/restyler")
    <*> optional (Env.var Env.str "RESTYLER_TAG" mempty)

envDatabaseConfig :: EnvParser PostgresConf
envDatabaseConfig = PostgresConf
    <$> (toConnStr <$> Env.var Env.nonempty "DATABASE_URL"
            (Env.def "postgres://postgres:password@localhost:5432/restyled"))
    <*> Env.var Env.auto "PGPOOLSIZE" (Env.def 10)
  where
    toConnStr = either error id . parsePGConnectionString

envRedisConfig :: EnvParser ConnectInfo
envRedisConfig = toConnectInfo <$> Env.var Env.nonempty "REDIS_URL"
    (Env.def "redis://localhost:6379")
  where
    toConnectInfo = either error id . parseRedisURL

envLogLevel :: EnvParser LogLevel
envLogLevel = toLogLevel <$> Env.var Env.str "LOG_LEVEL" (Env.def "info")
  where
    toLogLevel :: Text -> LogLevel
    toLogLevel t = case T.toLower t of
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
widgetFile = (if development then widgetFileReload else widgetFileNoReload) def

development :: Bool
development =
#if DEVELOPMENT
    True
#else
    False
#endif

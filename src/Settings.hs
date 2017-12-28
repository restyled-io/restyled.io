{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Settings where

import ClassyPrelude.Yesod hiding (Proxy)

import qualified Data.ByteString.Char8 as C8
import Data.Proxy
import qualified Data.Text as T
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..))
import qualified Env
import GitHub.Data
import GitHub.Data.Apps
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.RedisURL (parseRedisURL)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

data OAuthKeys = OAuthKeys
    { oauthKeysClientId :: Text
    , oauthKeysClientSecret :: Text
    }

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
    , appGitHubOAuthKeys :: OAuthKeys
    , appRestylerImage :: String
    , appRestylerTag :: Maybe String
    , appAdmins :: [Text]
    -- ^ +SECURITY_NOTE+ This relies on the fact that there is no authentication
    -- method available where users can enter an email themselves. Emails are
    -- taken only from GitHub credentials, and will only be present there if
    -- verified with GitHub.
    , appAllowDummyAuth :: Bool
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
    <*> (OAuthKeys
        <$> Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_ID" mempty
        <*> Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_SECRET" mempty)
    <*> Env.var Env.str "RESTYLER_IMAGE" (Env.def "restyled/restyler")
    <*> optional (Env.var Env.str "RESTYLER_TAG" mempty)
    <*> (map T.strip . T.splitOn "," <$> Env.var Env.str "ADMIN_EMAILS" (Env.def ""))
    <*> Env.switch "AUTH_DUMMY_LOGIN" mempty

envDatabaseConfig :: EnvParser PostgresConf
envDatabaseConfig = PostgresConf
    <$> Env.var Env.nonempty "DATABASE_URL" (Env.def defaultDatabaseURL)
    <*> Env.var Env.auto "PGPOOLSIZE" (Env.def 10)
  where
    defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

envRedisConfig :: EnvParser ConnectInfo
envRedisConfig = either error id . parseRedisURL
    <$> Env.var Env.nonempty "REDIS_URL" (Env.def "redis://localhost:6379")

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Settings
    ( OAuthKeys(..)
    , AppSettings(..)
    , HasSettings(..)
    , loadEnvSettings
    , loadEnvSettingsTest
    , allowsLevel
    , appSettingsIsDebug
    , widgetFile
    , appStaticDir
    , appFavicon
    , appRevision
    , marketplaceListingPath
    , requestLogger
    , makeStatic
    )
where

-- brittany-disable

import ClassyPrelude.Yesod

import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..), PortID(..), parseConnectInfo)
import Development.GitRev (gitCommitDate, gitHash)
import qualified Env
import Language.Haskell.TH.Syntax (Exp, Q)
import LoadEnv (loadEnvFrom)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (HostPreference)
#if DEVELOPMENT
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
#else
import Network.Wai.Middleware.RequestLogger (logStdout)
#endif
import RIO (Lens')
import SVCS.GitHub
import SVCS.GitHub.ApiClient (GitHubToken)
#if DEVELOPMENT
import Yesod.Default.Util (widgetFileReload)
#else
import Yesod.Default.Util (widgetFileNoReload)
#endif

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
    , appCopyright :: Text
    , appGitHubAppId :: GitHubAppId
    , appGitHubAppKey :: GitHubAppKey
    , appGitHubOAuthKeys :: Maybe OAuthKeys
    , appGitHubRateLimitToken :: GitHubToken
    , appGitLabOAuthKeys :: Maybe OAuthKeys
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
        , " db=[", redact $ C8.unpack $ pgConnStr appDatabaseConf, "]"
        , " redis=[", toURL appRedisConf, "]"
        , " restyler=[", restylerImage, "]"
        ]
      where
        redact :: String -> String
        redact s =
            let x = drop 2 $ dropWhile (/= '/') s
                user = takeWhile (/= ':') x
                rest = dropWhile (/= '@') x
            in "postgres://" <> user <> ":<redacted>" <> rest

        toURL :: ConnectInfo -> String
        toURL ConnInfo{..} = concat
            [ "redis://"
            , connectHost
            , maybe "" (const "<redacted>@") connectAuth
            , ":", showPortID connectPort
            , "/", show connectDatabase
            ]

        -- N.B. here is the cause of -fno-warn-deprecations. Redis is using the
        -- old Network interface, so we have no choice (other than giving up
        -- this bit of debugging).
        showPortID :: PortID -> String
        showPortID (Service s) = s
        showPortID (PortNumber p) = show p
        showPortID (UnixSocket s) = s

        restylerImage = appRestylerImage <> maybe "" (":" <>) appRestylerTag

class HasSettings env where
    settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
    settingsL = id

type EnvParser a = forall e.
    (Env.AsUnset e, Env.AsUnread e, Env.AsEmpty e) => Env.Parser e a


loadEnvSettings :: IO AppSettings
loadEnvSettings = do
#if DEVELOPMENT
    loadEnvFrom ".env.development"
#endif
    Env.parse id envSettings

loadEnvSettingsTest :: IO AppSettings
loadEnvSettingsTest = do
    loadEnvFrom ".env.test"
    Env.parse id envSettings

{-# ANN loadEnvSettings ("HLint: ignore Redundant do" :: String) #-}

envSettings :: EnvParser AppSettings
envSettings = AppSettings
    <$> envDatabaseConfig
    <*> envRedisConfig
    <*> Env.var Env.str "APPROOT" (Env.def "http://localhost:3000")
    <*> Env.var Env.str "HOST" (Env.def "*4")
    <*> Env.var Env.auto "PORT" (Env.def 3000)
    <*> Env.switch "IP_FROM_HEADER" mempty
    <*> envLogLevel
    <*> pure "Patrick Brisbin 2018-2019"
    <*> (mkGitHubAppId <$> Env.var Env.auto "GITHUB_APP_ID" mempty)
    <*> Env.var Env.nonempty "GITHUB_APP_KEY" mempty
    <*> (liftA2 OAuthKeys
        <$> optional (Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_ID" mempty)
        <*> optional (Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_SECRET" mempty))
    <*> Env.var Env.nonempty "GITHUB_RATE_LIMIT_TOKEN" mempty
    <*> (liftA2 OAuthKeys
        <$> optional (Env.var Env.nonempty "GITLAB_OAUTH_CLIENT_ID" mempty)
        <*> optional (Env.var Env.nonempty "GITLAB_OAUTH_CLIENT_SECRET" mempty))
    <*> Env.var Env.str "RESTYLER_IMAGE" (Env.def "restyled/restyler")
    <*> optional (Env.var Env.str "RESTYLER_TAG" mempty)
    <*> (map T.strip . T.splitOn "," <$> Env.var Env.str "ADMIN_EMAILS" (Env.def ""))
#if DOCKERIZED
    -- Don't even look for this setting if building the deployment image. We
    -- would need to both forget the compilation flag and accidentally set the
    -- ENV switch on production. Defense in depth.
    <*> pure False
#else
    <*> Env.switch "AUTH_DUMMY_LOGIN" mempty
#endif

envDatabaseConfig :: EnvParser PostgresConf
envDatabaseConfig = PostgresConf
    <$> Env.var Env.nonempty "DATABASE_URL" (Env.def defaultDatabaseURL)
    <*> Env.var Env.auto "PGPOOLSIZE" (Env.def 10)
  where
    defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

envRedisConfig :: EnvParser ConnectInfo
envRedisConfig = either error id . parseConnectInfo
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

appFavicon :: ByteString
appFavicon =
#if DEVELOPMENT
    $(embedFile "config/favicon-dev.ico")
#else
    $(embedFile "config/favicon.ico")
#endif

-- | Application revision
--
-- We add a static @config\/revision@ file in Docker builds, but we want to use
-- dynamic git operations in develompent.
--
appRevision :: ByteString
appRevision =
#if DOCKERIZED
    $(embedFile "config/revision")
#else
    $(gitHash) <> " - " <> $(gitCommitDate)
#endif

allowsLevel :: AppSettings -> LogLevel -> Bool
allowsLevel AppSettings{..} = (>= appLogLevel)

appSettingsIsDebug :: AppSettings -> Bool
appSettingsIsDebug = (`allowsLevel` LevelDebug)

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

marketplaceListingPath :: Text
marketplaceListingPath =
#if DEVELOPMENT
    "/marketplace_listing/stubbed"
#else
    "/marketplace_listing"
#endif

requestLogger :: Middleware
requestLogger =
#if DEVELOPMENT
    logStdoutDev
#else
    logStdout
#endif

makeStatic :: FilePath -> IO Static
makeStatic =
#if DEVELOPMENT
    staticDevel
#else
    static
#endif

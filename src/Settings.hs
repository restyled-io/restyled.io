{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Settings where

import ClassyPrelude.Yesod hiding (Builder, throw)

import Data.Text.Internal.Builder (Builder, toLazyText)
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..))
import GitHub.Model (GitHubId(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.PGDatabaseURL (parsePGConnectionString)
import Network.RedisURL (parseRedisURL)
import Network.Wai.Handler.Warp (HostPreference)
import Text.Shakespeare (RenderUrl)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Env
import qualified Text.Shakespeare.Text as ST

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
    , appGitHubAppId :: GitHubId
    , appGitHubAppKey :: Text
    , appGitHubOAuthKeys :: OAuthKeys
    , appRestylerExecutable :: FilePath
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
        , " restyler=", appRestylerExecutable
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
    <*> (GitHubId <$> Env.var Env.auto "GITHUB_APP_ID" mempty)
    <*> Env.var Env.nonempty "GITHUB_APP_KEY" mempty
    <*> (OAuthKeys
        <$> Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_ID" mempty
        <*> Env.var Env.nonempty "GITHUB_OAUTH_CLIENT_SECRET" mempty)
    <*> Env.var Env.str "RESTYLER_EXECUTABLE" (Env.def ".stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/restyler/restyler")
    <*> (map T.strip . T.splitOn "," <$> Env.var Env.str "ADMIN_EMAILS" (Env.def ""))
    <*> Env.switch "AUTH_DUMMY_LOGIN" mempty

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

-- | Sugar for the various @'Text'@ transformations need with @'textFile'@
fromTextTemplate :: (t -> Builder) -> t -> Text
fromTextTemplate t = toStrict . toLazyText . t

textFile :: FilePath -> Q Exp
textFile = ST.textFile
-- ^ if this works, then why is this ill-typed?
-- textFile = if development then ST.textFileReload else ST.textFile

renderTextUrl :: RenderUrl url -> ST.TextUrl url -> LT.Text
renderTextUrl = ST.renderTextUrl

development :: Bool
development =
#if DEVELOPMENT
    True
#else
    False
#endif

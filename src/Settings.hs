{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod hiding (throw)
import Database.Persist.Postgresql (PostgresConf(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.PGDatabaseURL (parsePGConnectionString)
import Network.Wai.Handler.Warp (HostPreference)
import Model.Base
import Yesod.Default.Util
#if DEVELOPMENT
    (widgetFileReload)
#else
    (widgetFileNoReload)
#endif

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Env

data AppSettings = AppSettings
    { appDatabaseConf :: PostgresConf
    , appRoot :: Text
    , appHost :: HostPreference
    , appPort :: Int
    , appIpFromHeader :: Bool
    , appLogLevel :: LogLevel
    , appMutableStatic :: Bool
    , appCopyright :: Text
    , appGitHubAppId :: GitHubId
    , appGitHubAppKey :: Text
    , appRestylerExecutable :: FilePath
    , appProcessWebhooks :: Bool
    }

instance Show AppSettings where
    show AppSettings{..} = concat
        [ "log_level=", show appLogLevel
        , " host=", show appHost
        , " port=", show appPort
        , " root=", show appRoot
        , " db=[", C8.unpack $ pgConnStr appDatabaseConf, "]"
        , " restyler=", appRestylerExecutable
        , " webhooks=", show appProcessWebhooks
        ]

type EnvParser a = forall e.
    (Env.AsUnset e, Env.AsUnread e, Env.AsEmpty e) => Env.Parser e a

loadEnvSettings :: IO AppSettings
loadEnvSettings = Env.parse id envSettings

envSettings :: EnvParser AppSettings
envSettings = AppSettings
    <$> envDatabaseConfig
    <*> Env.var Env.str "APPROOT" (Env.def "http://localhost:3000")
    <*> Env.var Env.str "HOST" (Env.def "*4")
    <*> Env.var Env.auto "PORT" (Env.def 3000)
    <*> Env.switch "IP_FROM_HEADER" mempty
    <*> envLogLevel
    <*> Env.switch "MUTABLE_STATIC" mempty
    <*> pure "Patrick Brisbin 2017"
    <*> (GitHubId <$> Env.var Env.auto "GITHUB_APP_ID" mempty)
    <*> Env.var Env.nonempty "GITHUB_APP_KEY" mempty
    <*> Env.var Env.str "RESTYLER_EXECUTABLE" (Env.def ".stack-work/dist/x86_64-linux-nopie/Cabal-1.24.2.0/build/restyler/restyler")
    <*> (not <$> Env.switch "DISCARD_WEBHOOKS" mempty)

envDatabaseConfig :: EnvParser PostgresConf
envDatabaseConfig = PostgresConf
    <$> (toConnStr <$> Env.var Env.nonempty "DATABASE_URL"
            (Env.def "postgres://postgres:password@localhost:5432/restyled"))
    <*> Env.var Env.auto "PGPOOLSIZE" (Env.def 10)
  where
    toConnStr = either error id . parsePGConnectionString

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
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

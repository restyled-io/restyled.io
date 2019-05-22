{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Settings
    ( OAuthKeys(..)

    -- * Settings
    , AppSettings(..)
    , HasSettings(..)
    , loadEnv

    -- * Compile-time choices
    , appSettingsIsDebug
    , appStaticDir
    , appRevision
    , widgetFile
    , marketplaceListingPath
    , requestLogger
    , makeStatic
    )
where

import ClassyPrelude.Yesod

import qualified Data.ByteString.Char8 as C8
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..), PortID(..))
import Development.GitRev (gitCommitDate, gitHash)
import Language.Haskell.TH.Syntax (Exp, Q)
import LoadEnv (loadEnvFrom)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (HostPreference)
import RIO (Lens')
import SVCS.GitHub
import SVCS.GitHub.ApiClient (GitHubToken)

#if DEVELOPMENT
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Yesod.Default.Util (widgetFileReload)
#else
import Network.Wai.Middleware.RequestLogger (logStdout)
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
    , appFavicon :: FilePath
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

loadEnv :: IO ()
loadEnv =
#if DEVELOPMENT
    loadEnvFrom ".env.development"
#else
    pure ()
#endif

allowsLevel :: AppSettings -> LogLevel -> Bool
allowsLevel AppSettings {..} = (>= appLogLevel)

appSettingsIsDebug :: AppSettings -> Bool
appSettingsIsDebug = (`allowsLevel` LevelDebug)

-- This value is needed in a pure context, and so can't read from ENV. It also
-- doesn't differ between environments, so we might as well harcode it.
appStaticDir :: FilePath
appStaticDir = "static"

-- brittany-disable-next-binding

-- appFavicon :: ByteString
-- appFavicon =
-- #if DEVELOPMENT
--     $(embedFile "config/favicon-dev.ico")
-- #else
--     $(embedFile "config/favicon.ico")
-- #endif

-- brittany-disable-next-binding

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

-- brittany-disable-next-binding

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

-- brittany-disable-next-binding

marketplaceListingPath :: Text
marketplaceListingPath =
#if DEVELOPMENT
    "/marketplace_listing/stubbed"
#else
    "/marketplace_listing"
#endif

-- brittany-disable-next-binding

requestLogger :: Middleware
requestLogger =
#if DEVELOPMENT
    logStdoutDev
#else
    logStdout
#endif

-- brittany-disable-next-binding

makeStatic :: FilePath -> IO Static
makeStatic =
#if DEVELOPMENT
    staticDevel
#else
    static
#endif

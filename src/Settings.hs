{-# LANGUAGE CPP #-}

module Settings
    ( OAuthKeys(..)

    -- * Runtime @'AppSettings'@
    , AppSettings(..)
    , HasSettings(..)

    -- * Compile-time settings
    , loadEnv
    , widgetFile
    )
where

import ClassyPrelude.Yesod

import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import LoadEnv (loadEnvFrom)
import Network.Wai.Handler.Warp (HostPreference)
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
    , appAllowDummyAuth :: Bool
    , appFavicon :: FilePath
    , appDetailedRequestLogger :: Bool
    , appMutableStatic :: Bool
    , appStaticDir :: FilePath
    , appStubMarketplaceListing :: Bool
    }

class HasSettings env where
    settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
    settingsL = id

-- brittany-disable-next-binding

loadEnv :: IO ()
loadEnv =
#if DEVELOPMENT
    loadEnvFrom ".env.development"
#else
    pure ()
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

{-# LANGUAGE CPP #-}

module Settings
    ( OAuthKeys(..)
    , addOAuth2Plugin

    -- * Runtime @'AppSettings'@
    , AppSettings(..)
    , HasSettings(..)
    , addAuthBackDoor

    -- * Compile-time settings
    , loadEnv
    , widgetFile
    )
where

import Restyled.Prelude

import Data.Default (def)
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Auth
import Yesod.Auth.Dummy

#if DEVELOPMENT
import LoadEnv (loadEnvFrom)
import Yesod.Default.Util (widgetFileReload)
#else
import Yesod.Default.Util (widgetFileNoReload)
#endif

data OAuthKeys = OAuthKeys
    { oauthKeysClientId :: Text
    , oauthKeysClientSecret :: Text
    }

addOAuth2Plugin
    :: (Text -> Text -> AuthPlugin app)
    -> Maybe OAuthKeys
    -> [AuthPlugin app]
    -> [AuthPlugin app]
addOAuth2Plugin mkPlugin = maybe id $ \OAuthKeys {..} ->
    (<> [mkPlugin oauthKeysClientId oauthKeysClientSecret])

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
    , appGitHubRateLimitToken :: ByteString
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

addAuthBackDoor
    :: YesodAuth app => AppSettings -> [AuthPlugin app] -> [AuthPlugin app]
addAuthBackDoor AppSettings {..} =
    if appAllowDummyAuth then (authDummy :) else id

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

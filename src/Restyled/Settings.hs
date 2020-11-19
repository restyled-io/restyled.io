{-# LANGUAGE CPP #-}

module Restyled.Settings
    ( OAuthKeys(..)
    , addOAuth2Plugin

    -- * Runtime @'AppSettings'@
    , AppSettings(..)
    , HasSettings(..)
    , loadSettings
    , addAuthBackDoor

    -- * Compile-time settings
    , widgetFile
    )
where

import Restyled.Prelude

import qualified Data.Default as Default (def)
import Database.Redis (ConnectInfo(..), defaultConnectInfo)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Restyled.Env
import Restyled.RestylerImage
import Restyled.Yesod hiding (LogLevel(..))
import RIO.Handler
import Yesod.Auth.Dummy
import Yesod.Core.Types (HandlerData(..))

#ifdef DEVELOPMENT
import Yesod.Default.UtilwidgetFileReload
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
    , appGitHubStudentsOAuthKeys :: Maybe OAuthKeys
    , appRestylerImage :: RestylerImage
    , appAdmins :: [Text]
    , appAllowDummyAuth :: Bool
    , appFavicon :: FilePath
    , appDetailedRequestLogger :: Bool
    , appMutableStatic :: Bool
    , appStaticDir :: FilePath
    , appStubMarketplaceListing :: Bool
    , appRestyleMachineLocal :: Bool
    , appRestyleMachineJobsMax :: Natural
    }

class HasSettings env where
    settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
    settingsL = id

instance HasSettings env => HasSettings (HandlerData child env) where
    settingsL = handlerEnvL . siteL . settingsL

addAuthBackDoor
    :: YesodAuth app => AppSettings -> [AuthPlugin app] -> [AuthPlugin app]
addAuthBackDoor AppSettings {..} =
    if appAllowDummyAuth then (authDummy :) else id

loadSettings :: IO AppSettings
loadSettings =
    parse
        $ AppSettings
        <$> (PostgresConf
            <$> var nonempty "DATABASE_URL" (def defaultDatabaseURL)
            <*> var auto "PGPOOLSIZE" (def 10)
            )
        <*> var connectInfo "REDIS_URL" (def defaultConnectInfo)
        <*> var str "APPROOT" (def "http://localhost:3000")
        <*> var str "HOST" (def "*4")
        <*> var auto "PORT" (def 3000)
        <*> var logLevel "LOG_LEVEL" (def LevelInfo)
        <*> var str "COPYRIGHT" (def "Patrick Brisbin 2018-2019")
        <*> var githubId "GITHUB_APP_ID" mempty
        <*> var nonempty "GITHUB_APP_KEY" mempty
        <*> (liftA2 OAuthKeys
            <$> optional (var nonempty "GITHUB_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITHUB_OAUTH_CLIENT_SECRET" mempty)
            )
        <*> var nonempty "GITHUB_RATE_LIMIT_TOKEN" mempty
        <*> (liftA2 OAuthKeys
            <$> optional (var nonempty "GITLAB_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITLAB_OAUTH_CLIENT_SECRET" mempty)
            )
        <*> (liftA2 OAuthKeys
            <$> optional
                    (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_ID" mempty)
            <*> optional
                    (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_SECRET" mempty
                    )
            )
        <*> (restylerImage
            <$> var str "RESTYLER_IMAGE" (def "restyled/restyler")
            <*> optional (var str "RESTYLER_TAG" mempty)
            )
        <*> var (splitOn ',') "ADMIN_EMAILS" (def [])
        <*> switch "AUTH_DUMMY_LOGIN" mempty
        <*> var str "FAVICON" (def "config/favicon.ico")
        <*> switch "DETAILED_REQUEST_LOGGER" mempty
        <*> switch "MUTABLE_STATIC" mempty
        <*> var nonempty "STATIC_DIR" (def "static")
        <*> switch "STUB_MARKETPLACE_LISTING" mempty
        <*> switch "RESTYLE_MACHINE_LOCAL" mempty
        <*> var auto "RESTYLE_MACHINE_JOBS_MAX" (def 3)

defaultDatabaseURL :: ByteString
defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

-- brittany-disable-next-binding

widgetFile :: String -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    Default.def

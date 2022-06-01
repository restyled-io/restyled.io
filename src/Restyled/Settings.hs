{-# LANGUAGE CPP #-}

module Restyled.Settings
    ( runAppLogging
    , OAuthKeys(..)
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
import Restyled.Queues
import Restyled.RestylerImage
import Restyled.Tracing.Config
import Restyled.Yesod
import Yesod.Auth.Dummy
import Yesod.Core.Types (HandlerData(..))

#ifdef DEVELOPMENT
import Yesod.Default.Util (widgetFileReload)
#else
import Yesod.Default.Util (widgetFileNoReload)
#endif

runAppLogging :: HasSettings app => app -> LoggingT m a -> m a
runAppLogging app f = do
    let level = app ^. settingsL . to appLogLevel
    runStdoutLoggingT $ filterLogger (const (>= level)) f

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
    , appStatementTimeout :: Maybe Integer
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
    , appMutableStatic :: Bool
    , appStaticDir :: FilePath
    , appStubMarketplaceListing :: Bool
    , appRestyleMachineLocal :: Bool
    , appRestyleMachineJobsMax :: Natural
    , appRequestTimeout :: Int
    , appRestylerLogGroup :: Text
    , appRestylerLogStreamPrefix :: Text
    , appAwsTrace :: Bool
    , appRestylerQueues :: Queues
    , appTracingConfig :: TracingConfig
    }

class HasSettings env where
    settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
    settingsL = id

instance HasSettings env => HasSettings (HandlerData child env) where
    settingsL = envL . siteL . settingsL

instance HasQueues AppSettings where
    queuesL = lens appRestylerQueues $ \x y -> x { appRestylerQueues = y }

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
            <*> var auto "PGPOOLSTRIPES" (def 1)
            <*> var auto "PGPOOLIDLETIMEOUT" (def 20)
            <*> var auto "PGPOOLSIZE" (def 10)
            )
        <*> optional (var auto "STATEMENT_TIMEOUT" mempty)
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
            <$> optional (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_SECRET" mempty)
            )
        <*> (restylerImage
            <$> var str "RESTYLER_IMAGE" (def "restyled/restyler:main")
            )
        <*> var (splitOn ',') "ADMIN_EMAILS" (def [])
        <*> switch "AUTH_DUMMY_LOGIN" mempty
        <*> var str "FAVICON" (def "config/favicon.ico")
        <*> switch "MUTABLE_STATIC" mempty
        <*> var nonempty "STATIC_DIR" (def "static")
        <*> switch "STUB_MARKETPLACE_LISTING" mempty
        <*> switch "RESTYLE_MACHINE_LOCAL" mempty
        <*> var auto "RESTYLE_MACHINE_JOBS_MAX" (def 3)
        <*> var auto "REQUEST_TIMEOUT" (def 20)
        <*> var nonempty "RESTYLER_LOG_GROUP" (def "restyled/dev/restyler")
        <*> var nonempty "RESTYLER_LOG_STREAM_PREFIX" (def "jobs/")
        <*> switch "AWS_TRACE" mempty
        <*> var (eitherReader readQueues) "RESTYLER_QUEUES" (def defaultQueues)
        <*> (TracingConfig
            <$> (DaemonSocket <$> optional (var nonempty "NEW_RELIC_DAEMON_SOCKET" mempty))
            <*> (AppName <$> var nonempty "NEW_RELIC_APP_NAME" (def "restyled.io"))
            <*> (LicenseKey <$$> optional (var nonempty "NEW_RELIC_LICENSE_KEY" mempty))
            <*> (TimeoutMs <$> var auto "NEW_RELIC_TIMEOUT_MS" (def 10000))
            <*> var nonempty "NEW_RELIC_LOG" (def "stdout")
            <*> var (eitherReader readTracingLogLevel) "NEW_RELIC_LOG_LEVEL" (def defaultTracingLogLevel)
            )

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

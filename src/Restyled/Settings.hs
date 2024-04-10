{-# LANGUAGE CPP #-}

module Restyled.Settings
  ( OAuthKeys (..)
  , addOAuth2Plugin

    -- * Runtime @'AppSettings'@
  , AppSettings (..)
  , HasSettings (..)
  , loadSettings
  , addAuthBackDoor

    -- * Compile-time settings
  , widgetFile
  )
where

import Restyled.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import qualified Data.Default as Default (def)
import Database.Redis (ConnectInfo (..), defaultConnectInfo)
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.Wai.Handler.Warp (HostPreference)
import Restyled.DB
import Restyled.Env
import Restyled.Queues
import Restyled.RestylerImage
import Restyled.Yesod
import Yesod.Auth.Dummy
import Yesod.Core.Types (HandlerData (..))

#ifdef DEVELOPMENT
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
  , appStatementTimeout :: Maybe Integer
  , appRedisConf :: ConnectInfo
  , appRoot :: Text
  , appHost :: HostPreference
  , appPort :: Int
  , appForceSSL :: Bool
  , appLogSettings :: LogSettings
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
  , appRestylerQueues :: Queues
  }

class HasSettings env where
  settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
  settingsL = id

instance HasSettings env => HasSettings (HandlerData child env) where
  settingsL = envL . siteL . settingsL

instance HasQueues AppSettings where
  queuesL = lens appRestylerQueues $ \x y -> x {appRestylerQueues = y}

addAuthBackDoor
  :: YesodAuth app => AppSettings -> [AuthPlugin app] -> [AuthPlugin app]
addAuthBackDoor AppSettings {..} =
  if appAllowDummyAuth then (authDummy :) else id

loadSettings :: IO AppSettings
loadSettings =
  parse
    $ AppSettings
    <$> postgresConf
    <*> optional (var auto "STATEMENT_TIMEOUT" mempty)
    <*> ( var connectInfo "REDIS_TLS_URL" mempty
            <|> var connectInfo "REDIS_URL" mempty
            <|> pure defaultConnectInfo
        )
    <*> var str "APPROOT" (def "http://localhost:3000")
    <*> var str "HOST" (def "*4")
    <*> var auto "PORT" (def 3000)
    <*> (not <$> switch "NO_FORCE_SSL" mempty)
    <*> LoggingEnv.parser
    <*> var str "COPYRIGHT" (def "2018 Patrick Brisbin")
    <*> var githubId "GITHUB_APP_ID" mempty
    <*> var nonempty "GITHUB_APP_KEY" mempty
    <*> ( liftA2 OAuthKeys
            <$> optional (var nonempty "GITHUB_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITHUB_OAUTH_CLIENT_SECRET" mempty)
        )
    <*> var nonempty "GITHUB_RATE_LIMIT_TOKEN" mempty
    <*> ( liftA2 OAuthKeys
            <$> optional (var nonempty "GITLAB_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITLAB_OAUTH_CLIENT_SECRET" mempty)
        )
    <*> ( liftA2 OAuthKeys
            <$> optional (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_ID" mempty)
            <*> optional (var nonempty "GITHUB_STUDENTS_OAUTH_CLIENT_SECRET" mempty)
        )
    <*> ( restylerImage
            <$> var str "RESTYLER_IMAGE" (def "restyled/restyler:edge")
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
    <*> var (eitherReader readQueues) "RESTYLER_QUEUES" (def defaultQueues)

{- FOURMOLU_DISABLE -}
widgetFile :: String -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    Default.def
{- FOURMOLU_ENABLE -}

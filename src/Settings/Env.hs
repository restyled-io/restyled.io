module Settings.Env
    ( loadEnvSettings
    )
where

import Prelude

import Control.Applicative (liftA2)
import Control.Monad.Logger (LogLevel(..))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Text (Text, pack)
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo, defaultConnectInfo, parseConnectInfo)
import Env hiding (Parser, splitOn)
import qualified Env
import Settings
import SVCS.GitHub.ApiClient (GitHubAppId, mkGitHubAppId)

loadEnvSettings :: IO AppSettings
loadEnvSettings = parse id settings

type Parser a = forall e.
    (AsUnset e, AsUnread e, AsEmpty e) => Env.Parser e a

settings :: Parser AppSettings
settings =
    AppSettings
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
        <*> var githubAppId "GITHUB_APP_ID" mempty
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
        <*> var str "RESTYLER_IMAGE" (def "restyled/restyler")
        <*> optional (var str "RESTYLER_TAG" mempty)
        <*> var (splitOn ',') "ADMIN_EMAILS" (def [])
        <*> switch "AUTH_DUMMY_LOGIN" mempty
        <*> var str "FAVICON" (def "config/favicon.ico")
        <*> switch "DETAILED_REQUEST_LOGGER" mempty

defaultDatabaseURL :: ByteString
defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

connectInfo :: AsUnread e => Reader e ConnectInfo
connectInfo = eitherReader parseConnectInfo

logLevel :: Reader e LogLevel
logLevel x = case map toLower x of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Right $ LevelOther $ pack x

githubAppId :: AsUnread e => Reader e GitHubAppId
githubAppId = fmap mkGitHubAppId . auto

splitOn :: Char -> Reader e [Text]
splitOn c = fmap (map pack) . Env.splitOn c

eitherReader :: AsUnread e => (String -> Either String a) -> Reader e a
eitherReader f = first unread . f

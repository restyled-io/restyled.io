-- | Local version of @envparse@
module Restyled.Env
    (
    -- * Re-exports
      Error
    , Parser
    , auto
    , def
    , nonempty
    , str
    , switch
    , var

    -- * Overrides
    , parse
    , splitOn

    -- * Extensions
    , eitherReader

    -- * Specialized @'Reader'@s
    , logLevel
    , connectInfo
    , githubId
    )
where

import Restyled.Prelude hiding (Reader)

import Database.Redis (ConnectInfo, parseConnectInfo)
import Env hiding (parse, splitOn)
import qualified Env

parse :: Parser Error a -> IO a
parse = Env.parse id

splitOn :: Char -> Reader e [Text]
splitOn c = (pack <$$>) . Env.splitOn c

eitherReader :: AsUnread e => (String -> Either String a) -> Reader e a
eitherReader f = first unread . f

logLevel :: Reader e LogLevel
logLevel x = case map toLower x of
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    "warn" -> Right LevelWarn
    "error" -> Right LevelError
    _ -> Right $ LevelOther $ pack x

connectInfo :: AsUnread e => Reader e ConnectInfo
connectInfo = eitherReader parseConnectInfo

githubId :: AsUnread e => Reader e (Id a)
githubId = fmap (mkId Proxy) . auto

-- | Local version of @envparse@
module Restyled.Env
  ( Error
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
  , connectInfo
  , githubId

    -- * Specialized @'Parser'@s
  , postgresConf
  ) where

import Restyled.Prelude hiding (Reader)

import Database.Redis (ConnectInfo, parseConnectInfo)
import Database.Redis.TLS (clientParamsNoVerify)
import qualified Database.Redis.TLS as TLS
import Env hiding (parse, splitOn)
import qualified Env
import Restyled.DB

parse :: Parser Error a -> IO a
parse = Env.parse id

splitOn :: Char -> Reader e [Text]
splitOn c = (pack <$$>) . Env.splitOn c

eitherReader :: AsUnread e => (String -> Either String a) -> Reader e a
eitherReader f = first unread . f

connectInfo :: AsUnread e => Reader e ConnectInfo
connectInfo = eitherReader $ \url ->
  -- No more <|> for Either, womp
  case (TLS.parseConnectInfo clientParamsNoVerify url, parseConnectInfo url) of
    (x@Right {}, _) -> x
    (_, y) -> y

githubId :: AsUnread e => Reader e (Id a)
githubId = fmap (mkId Proxy) . auto

postgresConf :: Parser Error PostgresConf
postgresConf =
  PostgresConf
    <$> var nonempty "DATABASE_URL" (def defaultDatabaseURL)
    <*> var auto "PGPOOLSTRIPES" (def 1)
    <*> var auto "PGPOOLIDLETIMEOUT" (def 20)
    <*> var auto "PGPOOLSIZE" (def 10)

defaultDatabaseURL :: ByteString
defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

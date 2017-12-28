module Network.RedisURL
    ( parseRedisURL
    ) where

import Control.Error.Util (note)
import Control.Monad (guard)
import Control.Monad.Plus (partial)
import qualified Data.ByteString.Char8 as C8
import Data.Maybe (fromMaybe)
import Database.Redis
import Network.HTTP.Base
import Network.URI (parseURI, uriPath, uriScheme)
import Text.Read (readMaybe)

-- | Parse a @'ConnectInfo'@ from a URL
--
-- Username is ignored, path is used to specify the database:
--
-- >>> parseRedisURL "redis://username:password@host:42/2"
-- Right (ConnInfo {connectHost = "host", connectPort = PortNumber 42, connectAuth = Just "password", connectDatabase = 2, connectMaxConnections = 50, connectMaxIdleTime = 30s, connectTimeout = Nothing})
--
-- N.B. invalid (non-integer) databases are ignored and revert to default:
--
-- >>> connectDatabase <$> parseRedisURL "redis://username:password@host:42/db"
-- Right 0
--
-- The scheme is validated, to prevent mixing up configurations:
--
-- >>> parseRedisURL "postgres://"
-- Left "Wrong scheme"
--
-- Beyond that, all values are optional. Omitted values are taken from
-- @'defaultConnectInfo'@:
--
-- >>> parseRedisURL "redis://"
-- Right (ConnInfo {connectHost = "localhost", connectPort = PortNumber 6379, connectAuth = Nothing, connectDatabase = 0, connectMaxConnections = 50, connectMaxIdleTime = 30s, connectTimeout = Nothing})
--
parseRedisURL :: String -> Either String ConnectInfo
parseRedisURL url = do
    uri <- note "Invalid URI" $ parseURI url
    note "Wrong scheme" $ guard $ uriScheme uri == "redis:"
    uriAuth <- note "Missing or invalid Authority"
        $ parseURIAuthority
        $ uriToAuthorityString uri

    return defaultConnectInfo
        { connectHost = fromMaybe (connectHost defaultConnectInfo)
            $ partial (not . null) $ host uriAuth
        , connectPort = maybe (connectPort defaultConnectInfo)
            (PortNumber . fromIntegral) $ port uriAuth
        , connectAuth = C8.pack <$> password uriAuth
        , connectDatabase = fromMaybe (connectDatabase defaultConnectInfo)
            $ readMaybe =<< partial (not . null) (dropWhile (== '/') $ uriPath uri)
        }

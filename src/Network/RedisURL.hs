module Network.RedisURL
    ( parseRedisURL
    ) where

import Control.Error.Util (note)
import Control.Monad (guard)
import Control.Monad.Plus (partial)
import Data.Maybe (fromMaybe)
import Database.Redis
import Network.HTTP.Base
import Network.URI (parseURI, uriPath, uriScheme)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C8

-- | Parse @redis://{ignored}:{password}@{host}:{port}/{db}@
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
        , connectPort = fromMaybe (connectPort defaultConnectInfo)
            $ PortNumber . fromIntegral <$> port uriAuth
        , connectAuth = C8.pack <$> password uriAuth
        , connectDatabase = fromMaybe (connectDatabase defaultConnectInfo)
            $ readMaybe =<< partial (not . null) (dropWhile (== '/') $ uriPath uri)
        }

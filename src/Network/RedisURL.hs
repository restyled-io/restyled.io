module Network.RedisURL
    ( parseRedisURL
    ) where

import Data.Maybe (fromMaybe)
import Database.Redis
import Network.HTTP.Base
    ( host
    , parseURIAuthority
    , password
    , port
    , uriToAuthorityString
    )
import Network.URI (parseURI, uriPath, uriScheme)
import Text.Read (readMaybe)

import qualified Data.ByteString.Char8 as C8

parseRedisURL :: String -> Either String ConnectInfo
parseRedisURL url = do
    uri <- checkP "URI" parseURI url
    uriAuth <- checkP "Authority" parseURIAuthority $ uriToAuthorityString uri
    checkScheme $ uriScheme uri

    return defaultConnectInfo
        { connectHost = fromMaybe (connectHost defaultConnectInfo)
            $ emptyToMaybe $ host uriAuth
        , connectPort = fromMaybe (connectPort defaultConnectInfo)
            $ PortNumber . fromIntegral <$> port uriAuth
        , connectAuth = C8.pack <$> password uriAuth
        , connectDatabase = fromMaybe (connectDatabase defaultConnectInfo)
            $ readMaybe =<< emptyToMaybe (dropChar '/' $ uriPath uri)

        -- Left as defaults:
        -- , connectMaxConnections = 0
        -- , connectMaxIdleTime = 50
        }

checkScheme :: String -> Either String ()
checkScheme "redis:" = return ()
checkScheme x = Left $
    "Invalid scheme: " ++ x ++ "//, expecting redis://"

checkP :: String -> (String -> Maybe a) -> String -> Either String a
checkP label p x = check ("Invalid " ++ label ++ ": " ++ x) $ p x

check :: String -> Maybe a -> Either String a
check msg = maybe (Left msg) Right

emptyToMaybe :: [a] -> Maybe [a]
emptyToMaybe [] = Nothing
emptyToMaybe x = Just x

dropChar :: Char -> String -> String
dropChar c (x:xs) | c == x = xs
dropChar _ x = x

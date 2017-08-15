module Network.PGDatabaseURL
    ( parsePGConnectionString
    ) where

import Prelude

import Data.Maybe (isJust)
import Data.String (IsString(..))
import Network.HTTP.Base
    ( host
    , parseURIAuthority
    , password
    , port
    , uriToAuthorityString
    , user
    )
import Network.URI (parseURI, uriPath, uriScheme)

data PGConnection = PGConnection
    { connUser :: Maybe String
    , connPassword :: Maybe String
    , connHost :: Maybe String
    , connPort :: Maybe Int
    , connDBname :: Maybe String
    }

parsePGConnectionString :: IsString a => String -> Either String a
parsePGConnectionString = fmap toConnectionString . parsePGDatabaseURL

parsePGDatabaseURL :: String -> Either String PGConnection
parsePGDatabaseURL url = do
    uri <- checkP "URI" parseURI url
    uriAuth <- checkP "Authority" parseURIAuthority $ uriToAuthorityString uri
    checkScheme $ uriScheme uri

    return PGConnection
        { connUser = user uriAuth
        , connPassword = password uriAuth
        , connHost = emptyToMaybe $ host uriAuth
        , connPort = port uriAuth
        , connDBname = emptyToMaybe $ dropChar '/' $ uriPath uri
        }

toConnectionString :: IsString a => PGConnection -> a
toConnectionString c = fromString $ unwords
    -- N.B. Pattern match is safe because of preceding filter
    $ map (\(k, Just v) -> k ++ "=" ++ v)
    $ filter (isJust . snd)
    [ ("user", connUser c)
    , ("password", connPassword c)
    , ("host", connHost c)
    , ("port", show <$> connPort c)
    , ("dbname", connDBname c)
    ]

checkScheme :: String -> Either String ()
checkScheme "postgres:" = return ()
checkScheme x = Left $
    "Invalid scheme: " ++ x ++ "//, expecting postgres://"

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

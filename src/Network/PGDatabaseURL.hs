module Network.PGDatabaseURL
    ( parsePGConnectionString
    ) where

import Control.Error.Util (note)
import Control.Monad (guard)
import Control.Monad.Plus (partial)
import Data.ByteString (ByteString)
import Network.HTTP.Base
import Network.URI (parseURI, uriPath, uriScheme)
import qualified Data.ByteString.Char8 as C8

-- | Parse @postgres://{user}:{password}@{host}:{port}/{db}@
parsePGConnectionString :: String -> Either String ByteString
parsePGConnectionString url = do
    uri <- note "Invalid URI" $ parseURI url
    note "Wrong scheme" $ guard $ uriScheme uri == "postgres:"
    uriAuth <- note "Missing or invalid Authority"
        $ parseURIAuthority
        $ uriToAuthorityString uri

    return $ C8.pack $ unwords
        [ k ++ "=" ++ v
        | (k, Just v) <-
            [ ("user", user uriAuth)
            , ("password", password uriAuth)
            , ("host", partial (not . null) $ host uriAuth)
            , ("port", show <$> port uriAuth)
            , ("dbname", partial (not . null) $ dropWhile (== '/') $ uriPath uri)
            ]
        ]

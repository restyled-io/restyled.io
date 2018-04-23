{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Installations
    ( createAccessToken
    , module GitHub.Data
    , module GitHub.Data.Apps
    , module GitHub.Data.AccessTokens
    ) where

import Prelude

import Control.Exception.Safe
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Data
import GitHub.Data.AccessTokens
import GitHub.Data.Apps
import Network.HTTP.Simple
import Network.HTTP.Types
import qualified Web.JWT as JWT

-- | 10 minutes
maxExpiration :: NominalDiffTime
maxExpiration = 10 * 60

createAccessToken :: Id App -> Text -> Id Installation -> IO AccessToken
createAccessToken githubAppId pem installationId = do
    jwt <- encodeJWT githubAppId (T.unpack pem)
    request <- parseRequest
        $ "POST https://api.github.com/installations/"
        <> show (untagId installationId)
        <> "/access_tokens"

    getResponseBody <$> httpJSON
        ( setRequestHeaders
            [ (hAccept, "application/vnd.github.machine-man-preview+json")
            , (hAuthorization, "Bearer " <> encodeUtf8 jwt)
            , (hUserAgent, "restyled-io")
            ]
            request
        )

encodeJWT :: Id App -> String -> IO JWT.JSON
encodeJWT githubAppId pem = do
    now <- getCurrentTime
    signer <- maybe (throwString "Invalid RSA data") pure
        =<< JWT.rsaKeySecret pem

    pure $ JWT.encodeSigned signer defaultClaimsSet
        { JWT.iat = numericDate now
        , JWT.exp = numericDate $ addUTCTime maxExpiration now
        , JWT.iss = JWT.stringOrURI $ T.pack $ show $ untagId githubAppId
        }
  where
    numericDate = JWT.numericDate . utcTimeToPOSIXSeconds

-- | Where'd the @'Default'@ instance go?
defaultClaimsSet :: JWT.JWTClaimsSet
defaultClaimsSet = JWT.JWTClaimsSet
    { JWT.iat = Nothing
    , JWT.exp = Nothing
    , JWT.iss = Nothing
    , JWT.sub = Nothing
    , JWT.aud = Nothing
    , JWT.nbf = Nothing
    , JWT.jti = Nothing
    , JWT.unregisteredClaims = JWT.ClaimsMap Map.empty
    }

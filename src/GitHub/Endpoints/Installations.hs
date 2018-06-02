{-# LANGUAGE OverloadedStrings #-}

module GitHub.Endpoints.Installations
    ( createAccessToken
    , module GitHub.Data
    , module GitHub.Data.Apps
    , module GitHub.Data.AccessTokens
    )
where

import Prelude

import Control.Applicative
import Control.Exception.Safe
import Data.Aeson
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Data
import GitHub.Data.AccessTokens
import GitHub.Data.Apps hiding (installationId)
import Network.HTTP.Simple
import Network.HTTP.Types
import qualified Web.JWT as JWT

newtype ErrorMessage = ErrorMessage Text

instance FromJSON ErrorMessage where
    parseJSON = withObject "TokenResponse" $ \o -> do
        msg <- o .: "message"
        url <- o .: "documentation_url"
        pure $ ErrorMessage $ msg <> " (see " <> url <> ")"

data TokenResponse
    = Token AccessToken
    | ErrorResponse ErrorMessage

instance FromJSON TokenResponse where
    parseJSON o = (Token <$> parseJSON o) <|> (ErrorResponse <$> parseJSON o)

tokenResponseToEither :: TokenResponse -> Either String AccessToken
tokenResponseToEither (Token x) = Right x
tokenResponseToEither (ErrorResponse (ErrorMessage x)) = Left $ T.unpack x

-- | Maximum expiration
--
-- 10 minutes is the documented maximum, but I can reliably trigger a "too far
-- in the future" error, which I assume is due to different clocks, so we do 5
-- minutes instead.
--
maxExpiration :: NominalDiffTime
maxExpiration = 5 * 60

-- | Create an Access Token for an installation of the given App
createAccessToken
    :: Id App -> Text -> Id Installation -> IO (Either String AccessToken)
createAccessToken githubAppId pem installationId =
    handleAny (pure . Left . show) $ do
        jwt <- encodeJWT githubAppId (T.unpack pem)
        request <-
            parseRequest
            $ "POST https://api.github.com/installations/"
            <> T.unpack (toPathPart installationId)
            <> "/access_tokens"

        tokenResponseToEither . getResponseBody <$> httpJSON
            (setRequestHeaders
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

    pure $ JWT.encodeSigned
        signer
        defaultClaimsSet
            { JWT.iat = numericDate now
            , JWT.exp = numericDate $ addUTCTime maxExpiration now
            , JWT.iss = JWT.stringOrURI $ T.pack $ show $ untagId githubAppId
            }
    where numericDate = JWT.numericDate . utcTimeToPOSIXSeconds

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

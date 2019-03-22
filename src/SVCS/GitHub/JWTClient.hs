{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SVCS.GitHub.JWTClient
    ( GitHubAppId
    , mkGitHubAppId
    , GitHubAppKey
    , githubGET
    , githubPOST
    , requestJWT

    -- * Long story
    , PersonalAccessToken
    , requestToken
    ) where

import Prelude

import Control.Exception.Safe
import Data.Aeson
import Data.Monoid ((<>))
import Data.Proxy
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Data (Id, mkId, untagId)
import GitHub.Data.Apps (App)
import Network.HTTP.Simple hiding (Proxy)
import Network.HTTP.Types
import qualified Web.JWT as JWT

type GitHubAppId = Id App

mkGitHubAppId :: Int -> GitHubAppId
mkGitHubAppId = mkId Proxy

newtype GitHubAppKey = AppKey Text
    deriving IsString

githubGET :: MonadThrow m => Text -> m Request
githubGET = parseRequest . ("GET https://api.github.com" <>) . unpack

githubPOST :: MonadThrow m => Text -> m Request
githubPOST = parseRequest . ("POST https://api.github.com" <>) . unpack

requestJWT :: FromJSON a => GitHubAppId -> GitHubAppKey -> Request -> IO a
requestJWT githubAppId (AppKey pem) request = do
    jwt <- encodeJWT githubAppId $ unpack pem
    getResponseBody <$> httpJSON
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
        mempty
            { JWT.iat = numericDate now
            , JWT.exp = numericDate $ addUTCTime maxExpiration now
            , JWT.iss = JWT.stringOrURI $ pack $ show $ untagId githubAppId
            }

-- | Maximum expiration
--
-- 10 minutes is the documented maximum, but I can reliably trigger a "too far
-- in the future" error, which I assume is due to different clocks, so we do 5
-- minutes instead.
--
maxExpiration :: NominalDiffTime
maxExpiration = 5 * 60

numericDate :: UTCTime -> Maybe JWT.NumericDate
numericDate = JWT.numericDate . utcTimeToPOSIXSeconds

newtype PersonalAccessToken = PersonalAccessToken Text
    deriving newtype IsString

requestToken :: FromJSON a => PersonalAccessToken -> Request -> IO a
requestToken (PersonalAccessToken token) request = getResponseBody <$> httpJSON
    (setRequestHeaders
        [ (hAccept, "application/vnd.github.machine-man-preview+json")
        , (hAuthorization, "token " <> encodeUtf8 token)
        , (hUserAgent, "restyled-io")
        ]
        request
    )

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SVCS.GitHub.AccessToken
    ( GitHubAppId
    , mkGitHubAppId
    , GitHubAppKey
    , githubInstallationToken
    ) where

import Prelude

import Control.Applicative
import Control.Exception.Safe
import Data.Aeson
import Data.Monoid ((<>))
import Data.Proxy
import Data.String
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Data (Id, mkId, toPathPart, untagId)
import GitHub.Data.AccessTokens (AccessToken(..))
import GitHub.Data.Apps (App)
import Network.HTTP.Simple hiding (Proxy)
import Network.HTTP.Types
import SVCS.Names
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

tokenResponseToEither :: TokenResponse -> Either String RepoAccessToken
tokenResponseToEither (Token x) = Right $ RepoAccessToken $ atToken x
tokenResponseToEither (ErrorResponse (ErrorMessage x)) = Left $ unpack x

-- | Maximum expiration
--
-- 10 minutes is the documented maximum, but I can reliably trigger a "too far
-- in the future" error, which I assume is due to different clocks, so we do 5
-- minutes instead.
--
maxExpiration :: NominalDiffTime
maxExpiration = 5 * 60

type GitHubAppId = Id App

mkGitHubAppId :: Int -> GitHubAppId
mkGitHubAppId = mkId Proxy

newtype GitHubAppKey = AppKey Text
    deriving IsString

-- | Create an Access Token for an installation of the given App
githubInstallationToken
    :: GitHubAppId
    -> GitHubAppKey
    -> InstallationId
    -> IO (Either String RepoAccessToken)
githubInstallationToken githubAppId (AppKey pem) installationId =
    handleAny (pure . Left . show) $ do
        jwt <- encodeJWT githubAppId $ unpack pem
        request <-
            parseRequest
            $ "POST https://api.github.com/installations/"
            <> unpack (toPathPart installationId)
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
        mempty
            { JWT.iat = numericDate now
            , JWT.exp = numericDate $ addUTCTime maxExpiration now
            , JWT.iss = JWT.stringOrURI $ pack $ show $ untagId githubAppId
            }

numericDate :: UTCTime -> Maybe JWT.NumericDate
numericDate = JWT.numericDate . utcTimeToPOSIXSeconds

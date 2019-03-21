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
import Data.Text (Text, unpack)
import GitHub.Data (toPathPart)
import GitHub.Data.AccessTokens (AccessToken(..))
import Network.HTTP.Simple hiding (Proxy)
import SVCS.GitHub.JWTClient
import SVCS.Names

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

-- | Create an Access Token for an installation of the given App
githubInstallationToken
    :: GitHubAppId
    -> GitHubAppKey
    -> InstallationId
    -> IO (Either String RepoAccessToken)
githubInstallationToken appId appKey installationId =
    handleAny (pure . Left . show) $ do
        request <-
            parseRequest
            $ "POST https://api.github.com/installations/"
            <> unpack (toPathPart installationId)
            <> "/access_tokens"

        tokenResponseToEither <$> requestJWT appId appKey request

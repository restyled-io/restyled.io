{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SVCS.GitLab.AccessToken
    ( gitlabRefreshedToken
    ) where

import Prelude

import Data.Text (Text)
import Network.HTTP.Client.TLS (getGlobalManager)
import qualified Network.OAuth.OAuth2 as OAuth2
import Settings (OAuthKeys(..))
import SVCS.Names
import URI.ByteString.Extension ()

gitlabRefreshedToken
    :: OAuthKeys
    -> Text -- Refresh Token
    -> IO (Either String RepoAccessToken)
gitlabRefreshedToken OAuthKeys {..} refreshToken = do
    manager <- getGlobalManager
    result <- OAuth2.refreshAccessToken manager oauth2
        $ OAuth2.RefreshToken refreshToken

    pure $ case result of
        Left _ -> Left "TODO"
        Right token ->
            Right $ RepoAccessToken $ OAuth2.atoken $ OAuth2.accessToken token
  where
    oauth2 = OAuth2.OAuth2
        { OAuth2.oauthClientId = oauthKeysClientId
        , OAuth2.oauthClientSecret = oauthKeysClientSecret
        , OAuth2.oauthOAuthorizeEndpoint = "https://gitlab.com/oauth/authorize"
        , OAuth2.oauthAccessTokenEndpoint = "https://gitlab.com/oauth/token"
        , OAuth2.oauthCallback = Nothing
        }

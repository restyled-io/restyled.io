{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.GithubApp
    ( oauth2GithubApp
    , module Yesod.Auth.OAuth2
    ) where

import Control.Exception (throwIO)
import Data.Text (Text)
import GitHub.Data (User(..), untagId)
import Yesod.Auth
import Yesod.Auth.OAuth2
import qualified Data.Text as T

oauth2GithubApp
    :: YesodAuth m
    => Text -- ^ Client ID
    -> Text -- ^ Client Secret
    -> AuthPlugin m
oauth2GithubApp clientId clientSecret = authOAuth2
    "github"
    ( OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
        , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
        , oauthCallback = Nothing
        }
    )
    (\m t -> do
        euser <- authGetJSON m (accessToken t) "https://api.github.com/user"

        case euser of
            Left err -> throwIO $ invalidProfileResponse "github" err
            Right user -> return Creds
                { credsPlugin = "github"
                , credsIdent = T.pack $ show $ untagId $ userId user
                , credsExtra = maybe [] (\e -> [("email", e)]) $ userEmail user
                }
    )

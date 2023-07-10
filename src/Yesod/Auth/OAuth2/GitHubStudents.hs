module Yesod.Auth.OAuth2.GitHubStudents
  ( oauth2GitHubStudents
  , pluginName
  ) where

import Prelude

import qualified Data.Text as T
import Yesod.Auth.OAuth2.Prelude

newtype User = User Int

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "id"

pluginName :: Text
pluginName = "github-students"

defaultScopes :: [Text]
defaultScopes = ["user:email"]

oauth2GitHubStudents :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2GitHubStudents clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://api.github.com/user"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = T.pack $ show userId
        , credsExtra = setExtra token userResponse
        }
 where
  oauth2 =
    OAuth2
      { oauth2ClientId = clientId
      , oauth2ClientSecret = Just clientSecret
      , oauth2AuthorizeEndpoint =
          "https://github.com/login/oauth/authorize"
            `withQuery` [scopeParam "," defaultScopes]
      , oauth2TokenEndpoint = "https://github.com/login/oauth/access_token"
      , oauth2RedirectUri = Nothing
      }

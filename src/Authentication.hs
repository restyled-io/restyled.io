{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication
    ( authenticateUser
    ) where

import Import.NoFoundation

import Data.Aeson
import Data.Aeson.Casing
import GitHub.Data (Id, Name)
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.OAuth2

data GitHubUser = GitHubUser
    { ghuEmail :: Text
    , ghuId :: Id User
    , ghuLogin :: Name User
    }
    deriving (Eq, Show, Generic)

instance FromJSON GitHubUser where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

authenticateUser
    :: AuthId site ~ UserId => Creds site -> DB (AuthenticationResult site)
authenticateUser creds@Creds {..} = do
    mUserId <- entityKey <$$> getBy (UniqueUser credsPlugin credsIdent)
    logDebugN $ "Existing User Id: " <> tshow (toPathPiece <$> mUserId)

    let eGhUser = getUserResponseJSON creds
    logDebugN $ "GitHub user: " <> tshow eGhUser

    case (mUserId, eGhUser) of
        (Nothing, Left err) -> do
            logWarnN $ "Error parsing user response: " <> pack err
            pure $ UserError $ IdentifierNotFound "GitHub OAuth2 response"

        (Nothing, Right ghUser) -> createFromGitHub creds ghUser
        (Just userId, Right ghUser) -> updateFromGitHub creds userId ghUser

        -- Probably testing via auth/dummy, just authenticate
        (Just userId, Left _) -> pure $ Authenticated userId

createFromGitHub
    :: AuthId site ~ UserId
    => Creds site
    -> GitHubUser
    -> DB (AuthenticationResult site)
createFromGitHub Creds {..} GitHubUser {..} = Authenticated <$> insert User
    { userEmail = ghuEmail
    , userGithubUserId = Just ghuId
    , userGithubUsername = Just ghuLogin
    , userCredsIdent = credsIdent
    , userCredsPlugin = credsPlugin
    }

updateFromGitHub
    :: AuthId site ~ UserId
    => Creds site
    -> UserId
    -> GitHubUser
    -> DB (AuthenticationResult site)
updateFromGitHub Creds {..} userId GitHubUser {..} =
    Authenticated userId <$ updateWhere
        [ UserId ==. userId
        , UserCredsIdent ==. credsIdent
        , UserCredsPlugin ==. credsPlugin
        ]
        [ UserEmail =. ghuEmail
        , UserGithubUserId =. Just ghuId
        , UserGithubUsername =. Just ghuLogin
        ]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Authentication
    ( authenticateUser
    ) where

import Import.NoFoundation

import Cache
import Data.Aeson
import Data.Aeson.Casing
import Yesod.Auth
import Yesod.Auth.Message
import Yesod.Auth.OAuth2

data GitHubUser = GitHubUser
    { ghuEmail :: Text
    , ghuId :: GitHubUserId
    , ghuLogin :: GitHubUserName
    }
    deriving (Eq, Show, Generic)

instance FromJSON GitHubUser where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

authenticateUser
    :: (AuthId site ~ UserId, MonadCache m, MonadHandler m)
    => Creds site
    -> SqlPersistT m (AuthenticationResult site)
authenticateUser creds@Creds {..} = do
    mUserId <- findUserForCreds creds

    logDebugN $ "Authentication credentials: " <> tshow creds
    logDebugN $ "Existing User: " <> maybe "none" toPathPiece mUserId

    case (mUserId, userFromCreds creds) of
        -- Existing user, good data: update accordingly
        (Just userId, Right user) ->
            Authenticated userId <$ replaceUser userId user

        -- Existing user, no data: authenticate anyway
        (Just userId, Left _) -> pure $ Authenticated userId

        -- New user, good data: create accordingly
        (Nothing, Right user) -> Authenticated <$> insert user

        -- No user, no data: bail
        (Nothing, Left err) -> do
            logWarnN $ "OAuth2 response error: " <> pack err
            pure $ UserError $ IdentifierNotFound "OAuth2 Response"

findUserForCreds :: MonadIO m => Creds site -> SqlPersistT m (Maybe UserId)
findUserForCreds creds@Creds {..} =
    (entityKey <$$>) . runMaybeT $ getMatchingCreds <|> getMatchingEmail
  where
    getMatchingCreds = MaybeT $ getBy $ UniqueUser credsPlugin credsIdent
    getMatchingEmail = do
        User {..} <- liftMaybe $ hush $ userFromCreds creds
        MaybeT $ selectFirst [UserEmail ==. userEmail] []

-- | A version of 'replace' that avoids clobbering when possible
replaceUser :: MonadIO m => UserId -> User -> SqlPersistT m ()
replaceUser userId User {..} = update userId $ catMaybes
    [ Just $ UserEmail =. userEmail
    , (UserGithubUserId =.) . Just <$> userGithubUserId
    , (UserGithubUsername =.) . Just <$> userGithubUsername
    ]

userFromCreds :: Creds site -> Either String User
userFromCreds creds = do
    GitHubUser {..} <- getUserResponseJSON creds

    pure User
        { userEmail = ghuEmail
        , userGithubUserId = Just ghuId
        , userGithubUsername = Just ghuLogin
        , userCredsIdent = credsIdent creds
        , userCredsPlugin = credsPlugin creds
        }

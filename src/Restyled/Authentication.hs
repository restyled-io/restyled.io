module Restyled.Authentication
    ( authenticateUser
    )
where

import Restyled.Prelude

import Network.OAuth.OAuth2 as OAuth2
import Restyled.Models
import Restyled.Yesod

data GitHubUser = GitHubUser
    { ghuEmail :: Maybe Text
    , ghuId :: GitHubUserId
    , ghuLogin :: GitHubUserName
    }
    deriving (Eq, Show, Generic)

instance FromJSON GitHubUser where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GitLabUser = GitLabUser
    { gluEmail :: Maybe Text
    , gluId :: GitLabUserId
    , glUsername :: GitLabUserName
    }
    deriving (Eq, Show, Generic)

instance FromJSON GitLabUser where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

authenticateUser
    :: (AuthId site ~ UserId, MonadLogger m, MonadIO m)
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
            pure $ UserError $ IdentifierNotFound "Unexpected OAuth2 Response"

findUserForCreds :: MonadIO m => Creds site -> SqlPersistT m (Maybe UserId)
findUserForCreds creds@Creds {..} =
    (entityKey <$$>) . runMaybeT $ getMatchingCreds <|> getMatchingEmail
  where
    getMatchingCreds = MaybeT $ getBy $ UniqueUser credsPlugin credsIdent
    getMatchingEmail = do
        User {..} <- hushT $ liftEither $ userFromCreds creds
        MaybeT $ selectFirst [UserEmail ==. userEmail, UserEmail !=. Nothing] []

-- | A version of 'replace' that avoids clobbering when possible
replaceUser :: MonadIO m => UserId -> User -> SqlPersistT m ()
replaceUser userId User {..} = update userId $ catMaybes
    [ (UserEmail =.) . Just <$> userEmail
    , (UserGithubUserId =.) . Just <$> userGithubUserId
    , (UserGithubUsername =.) . Just <$> userGithubUsername
    , (UserGitlabUserId =.) . Just <$> userGitlabUserId
    , (UserGitlabUsername =.) . Just <$> userGitlabUsername
    , (UserGitlabAccessToken =.) . Just <$> userGitlabAccessToken
    , (UserGitlabRefreshToken =.) . Just <$> userGitlabRefreshToken
    , Just $ UserCredsIdent =. userCredsIdent
    , Just $ UserCredsPlugin =. userCredsPlugin
    ]

userFromCreds :: Creds site -> Either String User
userFromCreds creds = case credsPlugin creds of
    "github" -> do
        GitHubUser {..} <- getUserResponseJSON creds

        pure User
            { userEmail = ghuEmail
            , userGithubUserId = Just ghuId
            , userGithubUsername = Just ghuLogin
            , userGitlabUserId = Nothing
            , userGitlabUsername = Nothing
            , userGitlabAccessToken = Nothing
            , userGitlabRefreshToken = Nothing
            , userCredsIdent = credsIdent creds
            , userCredsPlugin = credsPlugin creds
            }

    "gitlab" -> do
        GitLabUser {..} <- getUserResponseJSON creds

        pure User
            { userEmail = gluEmail
            , userGithubUserId = Nothing
            , userGithubUsername = Nothing
            , userGitlabUserId = Just gluId
            , userGitlabUsername = Just glUsername
            , userGitlabAccessToken = OAuth2.atoken <$> getAccessToken creds
            , userGitlabRefreshToken = OAuth2.rtoken <$> getRefreshToken creds
            , userCredsIdent = credsIdent creds
            , userCredsPlugin = credsPlugin creds
            }

    x -> Left $ unpack $ "Unexpected AuthPlugin: " <> x

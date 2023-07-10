module Restyled.Authentication
  ( authenticateUser
  ) where

import Restyled.Prelude

import qualified Data.Text as T
import Network.OAuth.OAuth2 as OAuth2
import Restyled.GitHubStudents (giftGitHubStudents)
import Restyled.Models
import Restyled.Yesod

data GitHubUser = GitHubUser
  { ghuEmail :: Maybe Text
  , ghuId :: GitHubUserId
  , ghuLogin :: GitHubUserName
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GitHubUser where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GitLabUser = GitLabUser
  { gluEmail :: Maybe Text
  , gluId :: GitLabUserId
  , glUsername :: GitLabUserName
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GitLabUser where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

authenticateUser
  :: (AuthId site ~ UserId, MonadLogger m, MonadIO m)
  => Creds site
  -> SqlPersistT m (AuthenticationResult site)
authenticateUser creds = do
  mUserId <- findUserForCreds creds

  logDebug $ "Authentication" :# ["credentials" .= show @Text creds]
  logDebug
    $ maybe
      "No existing user"
      (\userId -> "Existing user" :# ["id" .= userId])
      mUserId

  case (mUserId, userFromCreds creds) of
    -- Existing user, good data: update accordingly
    (Just userId, Right user) -> do
      giftGitHubStudents creds
      Authenticated userId <$ replaceUser userId user

    -- Existing user, no data: authenticate anyway
    (Just userId, Left _) -> pure $ Authenticated userId
    -- New user, good data: create accordingly
    (Nothing, Right user) -> do
      giftGitHubStudents creds
      Authenticated <$> insert user

    -- No user, no data: bail
    (Nothing, Left err) -> do
      logWarn $ "OAuth2 response error" :# ["error" .= err]
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
replaceUser userId User {..} =
  update userId
    $ catMaybes
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
userFromCreds creds
  | "github" `T.isPrefixOf` credsPlugin creds = do
      GitHubUser {..} <- getUserResponseJSON creds

      pure
        User
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
  | "gitlab" == credsPlugin creds = do
      GitLabUser {..} <- getUserResponseJSON creds

      pure
        User
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
  | otherwise = Left $ unpack $ "Unexpected AuthPlugin: " <> credsPlugin creds

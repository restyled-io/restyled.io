{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restyled.GitHubStudents
    ( giftGitHubStudents
    )
where

import Restyled.Prelude hiding (id)

import Control.Lens ((^?))
import Data.Aeson.Lens
import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import qualified Network.OAuth.OAuth2 as OAuth2
import Restyled.Backend.DiscountMarketplacePlan
import Restyled.Yesod
import qualified Yesod.Auth.OAuth2.GitHubStudents as GitHubStudents

data GitHubStudent = GitHubStudent
    { id :: GitHubUserId
    , login :: GitHubUserName
    }
    deriving stock Generic
    deriving anyclass FromJSON

giftGitHubStudents
    :: (MonadIO m, MonadLogger m) => Creds site -> SqlPersistT m ()
giftGitHubStudents creds = void $ runMaybeT $ do
    guard $ credsPlugin creds == GitHubStudents.pluginName
    student <- hoistMaybe $ hush $ getUserResponseJSON creds
    accessToken <- hoistMaybe $ getAccessToken creds

    lift $ do
        verified <- verifyIsGitHubStudent accessToken
        logInfoN
            $ "Verified GitHub Student "
            <> displayGitHubStudent student verified
        handleGitHubStudent student verified

displayGitHubStudent :: GitHubStudent -> Bool -> Text
displayGitHubStudent GitHubStudent { id, login } verified = T.unwords
    [ "github_id=" <> toPathPiece id
    , "github_login=" <> toPathPiece login
    , "verified=" <> tshow verified
    ]

handleGitHubStudent :: MonadIO m => GitHubStudent -> Bool -> SqlPersistT m ()
handleGitHubStudent GitHubStudent { id, login } verified
    | verified = giftDiscountMarketplacePlan id login
    | otherwise = ungiftDiscountMarketplacePlan id login

verifyIsGitHubStudent :: MonadIO m => OAuth2.AccessToken -> m Bool
verifyIsGitHubStudent token = do
    body <- getResponseBody <$> httpBS req
    pure $ fromMaybe False $ body ^? key "student" . _Bool
  where
    req =
        addRequestHeader hAccept "application/json"
            $ addRequestHeader hAuthorization auth
            $ parseRequest_ "https://education.github.com/api/user"
    auth = "token " <> encodeUtf8 (OAuth2.atoken token)

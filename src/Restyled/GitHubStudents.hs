{-# LANGUAGE NamedFieldPuns #-}

module Restyled.GitHubStudents
    ( giftGitHubStudents
    , githubStudentsPlan
    )
where

import Restyled.Prelude hiding (id)

import Data.Aeson.Lens
import qualified Data.Text as T
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import qualified Network.OAuth.OAuth2 as OAuth2
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Time
import Restyled.Yesod
import qualified Yesod.Auth.OAuth2.GitHubStudents as GitHubStudents

data GitHubStudent = GitHubStudent
    { id :: GitHubUserId
    , login :: GitHubUserName
    , email :: Maybe Text
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
handleGitHubStudent GitHubStudent { id, login, email } verified =
    findOrCreateMarketplacePlan githubStudentsPlan >>= go
  where
    go planId
        | verified = addAccount planId
        | otherwise = removeAccount planId

    addAccount (Entity planId _) = do
        nextYear <- addTime (Years 1) <$> getCurrentTime
        void $ upsert
            MarketplaceAccount
                { marketplaceAccountGithubId = Just id
                , marketplaceAccountGithubLogin = login
                , marketplaceAccountMarketplacePlan = planId
                , marketplaceAccountGithubType = "User"
                , marketplaceAccountEmail = email
                , marketplaceAccountBillingEmail = email
                , marketplaceAccountExpiresAt = Just nextYear
                }
            [ MarketplaceAccountMarketplacePlan =. planId
            , MarketplaceAccountExpiresAt =. Just nextYear
            ]

    removeAccount (Entity planId _) = deleteWhere
        [ MarketplaceAccountGithubLogin ==. login
        , MarketplaceAccountMarketplacePlan ==. planId
        ]

verifyIsGitHubStudent
    :: (MonadIO m, MonadLogger m) => OAuth2.AccessToken -> m Bool
verifyIsGitHubStudent token = do
    body <- getResponseBody <$> httpBS req

    case body ^? key "student" . _Bool of
        Nothing -> do
            logWarnN $ "Unexpected response: " <> tshow body
            pure False
        Just x -> pure x
  where
    req =
        addRequestHeader hAccept "application/json"
            $ addRequestHeader hAuthorization auth
            $ parseRequest_ "https://education.github.com/api/user"
    auth = "token " <> encodeUtf8 (OAuth2.atoken token)

githubStudentsPlan :: MarketplacePlan
githubStudentsPlan = MarketplacePlan
    { marketplacePlanGithubId = Nothing
    , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceUnlimited
    , marketplacePlanName = "GitHub Students"
    , marketplacePlanDescription = "Free Unlimited for verified GitHub Students"
    , marketplacePlanMonthlyRevenue = fromCents 0
    }

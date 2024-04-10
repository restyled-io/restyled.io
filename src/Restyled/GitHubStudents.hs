{-# LANGUAGE NamedFieldPuns #-}

module Restyled.GitHubStudents
  ( giftGitHubStudents
  , githubStudentsPlan
  ) where

import Restyled.Prelude hiding (id)

import Data.Aeson.Lens
import Lens.Micro ((^?))
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import qualified Network.OAuth.OAuth2 as OAuth2
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Time
import Restyled.UsCents
import Restyled.Yesod
import qualified Yesod.Auth.OAuth2.GitHubStudents as GitHubStudents

data GitHubStudent = GitHubStudent
  { id :: GitHubUserId
  , login :: GitHubUserName
  , email :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

giftGitHubStudents
  :: (MonadIO m, MonadLogger m) => Creds site -> SqlPersistT m ()
giftGitHubStudents creds = void $ runMaybeT $ do
  guard $ credsPlugin creds == GitHubStudents.pluginName
  student <- hoistMaybe $ hush $ getUserResponseJSON creds
  accessToken <- hoistMaybe $ getAccessToken creds

  lift $ do
    verified <- verifyIsGitHubStudent accessToken
    handleGitHubStudent student verified

handleGitHubStudent
  :: (MonadIO m, MonadLogger m)
  => GitHubStudent
  -> Verified
  -> SqlPersistT m ()
handleGitHubStudent GitHubStudent {id, login, email} verified = do
  planId <- findOrCreateMarketplacePlan githubStudentsPlan

  case verified of
    Verified -> do
      logInfo $ "Verified GitHub Student" :# context <> ["verified" .= True]
      addAccount planId
    Unverified -> do
      logInfo $ "Verified GitHub Student" :# context <> ["verified" .= False]
      removeAccount planId
    VerifyError token req resp -> do
      logWarn
        $ "Verified GitHub Student"
        :# ( context
              <> [ "verify-error"
                    .= object
                      [ "token" .= token
                      , "request" .= show @Text req
                      , "response" .= show @Text resp
                      ]
                 ]
           )
 where
  -- Avoid MonadMask by faking withThreadContext
  context = ["githubId" .= id, "githubLogin" .= login]

  addAccount (Entity planId _) = do
    nextYear <- addTime (Years 1) <$> getCurrentTime
    void
      $ upsert
        MarketplaceAccount
          { marketplaceAccountGithubId = Just id
          , marketplaceAccountGithubLogin = login
          , marketplaceAccountMarketplacePlan = planId
          , marketplaceAccountGithubType = "User"
          , marketplaceAccountEmail = email
          , marketplaceAccountBillingEmail = email
          , marketplaceAccountTrialEndsAt = Nothing
          , marketplaceAccountExpiresAt = Just nextYear
          }
        [ MarketplaceAccountMarketplacePlan =. planId
        , MarketplaceAccountExpiresAt =. Just nextYear
        ]

  removeAccount (Entity planId _) =
    deleteWhere
      [ MarketplaceAccountGithubLogin ==. login
      , MarketplaceAccountMarketplacePlan ==. planId
      ]

data Verified
  = Verified
  | Unverified
  | VerifyError Text Request (Response ByteString)

verifyIsGitHubStudent :: MonadIO m => OAuth2.AccessToken -> m Verified
verifyIsGitHubStudent token = do
  resp <- httpBS req

  let body = getResponseBody resp

  pure $ case body ^? key "student" . _Bool of
    Nothing -> VerifyError tokenT req resp
    Just True -> Verified
    Just False -> Unverified
 where
  req =
    addRequestHeader hAccept "application/json"
      $ addRequestHeader hAuthorization auth
      $ parseRequest_ "https://education.github.com/api/user"
  auth = "token " <> encodeUtf8 tokenT
  tokenT = OAuth2.atoken token

githubStudentsPlan :: MarketplacePlan
githubStudentsPlan =
  MarketplacePlan
    { marketplacePlanGithubId = Nothing
    , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceUnlimited
    , marketplacePlanName = "GitHub Students"
    , marketplacePlanDescription = "Free Unlimited for verified GitHub Students"
    , marketplacePlanMonthlyRevenue = fromCents 0
    , marketplacePlanRetired = False
    , marketplacePlanCpuShares = Nothing
    , marketplacePlanMemory = Nothing
    }

{-# LANGUAGE TemplateHaskell #-}

module Handler.Profile
    ( getProfileR
    )
where

import Import

import Foundation
import Routes
import Yesod
import Yesod.Auth

data Org = Org
    { orgId :: GitHubUserId
    , orgLogin :: GitHubUserName
    , orgAvatarUrl :: Text
    }
    deriving Generic

instance FromJSON Org where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

getProfileR :: Handler Html
getProfileR = do
    Entity _ user <- requireAuth
    mUserIdentity <- runDB $ fetchGitHubIdentityForUser user

    orgs <- requestUserOrgs user
    orgIdentities <- traverse (runDB . fetchGitHubIdentityForOrg) orgs

    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")

requestUserOrgs :: User -> Handler [Org]
requestUserOrgs = maybe (pure []) requestUserNameOrgs . userGithubUsername

requestUserNameOrgs :: GitHubUserName -> Handler [Org]
requestUserNameOrgs username = do
    settings <- getsYesod appSettings
    requestOrgs settings username `catchAny` \ex -> do
        logWarnN $ tshow ex
        pure []

requestOrgs :: MonadIO m => AppSettings -> GitHubUserName -> m [Org]
requestOrgs AppSettings {..} username = liftIO $ do
    request <- githubGET $ "/users/" <> toPathPart username <> "/orgs"
    -- TODO: https://github.community/t5/GitHub-API-Development-and/How-to-list-a-user-s-organizations/m-p/20864#M1208
    -- requestJWT appGitHubAppId appGitHubAppKey request
    requestToken appGitHubRateLimitToken request

data GitHubIdentity = GitHubIdentity
    { ghiUserName :: GitHubUserName
    , ghiAvatarUrl :: Text -- ^ TODO: newtype?
    , ghiKnownRepos :: [Entity Repo]
    , ghiMarketplacePlan :: Maybe MarketplacePlan
    }

fetchGitHubIdentityForOrg :: MonadIO m => Org -> SqlPersistT m GitHubIdentity
fetchGitHubIdentityForOrg Org {..} =
    GitHubIdentity orgLogin orgAvatarUrl
        <$> fetchReposByOwnerName (userToOwnerName orgLogin)
        <*> fetchMarketplacePlanByLogin orgLogin

fetchGitHubIdentityForUser
    :: MonadIO m => User -> SqlPersistT m (Maybe GitHubIdentity)
fetchGitHubIdentityForUser user@User {..} =
    for mDetails $ \(githubId, githubUsername) ->
        GitHubIdentity githubUsername (avatarUrl githubId)
            <$> fetchReposByOwnerName (userToOwnerName githubUsername)
            <*> fetchMarketplacePlanForUser user
  where
    mDetails = (,) <$> userGithubUserId <*> userGithubUsername
    avatarUrl gid =
        "https://avatars0.githubusercontent.com/u/" <> toPathPart gid <> "?v=4"

-- brittany-disable-next-binding

githubIdentityCard :: GitHubIdentity -> Widget
githubIdentityCard GitHubIdentity {..} =
    $(widgetFile "profile/github-identity-card")

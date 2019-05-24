{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Profile
    ( getProfileR
    )
where

import Restyled.Prelude

import qualified Data.Vector as V
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.Yesod

getProfileR :: Handler Html
getProfileR = do
    Entity _ user <- requireAuth
    mUserIdentity <- runDB $ fetchGitHubIdentityForUser user

    orgs <- requestUserOrgs user
    orgIdentities <- traverse (runDB . fetchGitHubIdentityForOrg) orgs

    defaultLayout $ do
        setTitle "Profile"
        $(widgetFile "profile")

requestUserOrgs :: User -> Handler [SimpleOrganization]
requestUserOrgs = maybe (pure []) requestUserNameOrgs . userGithubUsername

requestUserNameOrgs :: GitHubUserName -> Handler [SimpleOrganization]
requestUserNameOrgs username = do
    auth <- getsYesod $ Just . OAuth . appGitHubRateLimitToken . view settingsL
    result <- liftIO $ publicOrganizationsFor' auth username

    case result of
        Left err -> [] <$ logWarnN (tshow err)
        Right orgs -> pure $ V.toList orgs

data GitHubIdentity = GitHubIdentity
    { ghiUserName :: GitHubUserName
    , ghiAvatarUrl :: Text -- ^ TODO: newtype?
    , ghiKnownRepos :: [Entity Repo]
    , ghiMarketplacePlan :: Maybe MarketplacePlan
    }

fetchGitHubIdentityForOrg
    :: MonadIO m => SimpleOrganization -> SqlPersistT m GitHubIdentity
fetchGitHubIdentityForOrg SimpleOrganization {..} =
    GitHubIdentity orgLogin orgAvatarUrl
        <$> fetchReposByOwnerName (nameToName orgLogin)
        <*> fetchMarketplacePlanByLogin orgLogin
  where
    orgLogin :: GitHubUserName
    orgLogin = nameToName simpleOrganizationLogin

    orgAvatarUrl :: Text
    orgAvatarUrl = getUrl simpleOrganizationAvatarUrl

fetchGitHubIdentityForUser
    :: MonadIO m => User -> SqlPersistT m (Maybe GitHubIdentity)
fetchGitHubIdentityForUser user@User {..} =
    for mDetails $ \(githubId, githubUsername) ->
        GitHubIdentity githubUsername (avatarUrl githubId)
            <$> fetchReposByOwnerName (nameToName githubUsername)
            <*> fetchMarketplacePlanForUser user
  where
    mDetails = (,) <$> userGithubUserId <*> userGithubUsername
    avatarUrl gid =
        "https://avatars0.githubusercontent.com/u/" <> toPathPart gid <> "?v=4"

-- brittany-disable-next-binding

githubIdentityCard :: GitHubIdentity -> Widget
githubIdentityCard GitHubIdentity {..} =
    $(widgetFile "profile/github-identity-card")

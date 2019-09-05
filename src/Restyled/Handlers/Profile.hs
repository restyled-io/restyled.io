{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Profile
    ( getProfileR

    -- * Exported for use in tests
    , GitHubOrg(..)
    , githubOrgsCacheKey
    )
where

import Restyled.Prelude

import qualified Data.Vector as V
import Restyled.Cache
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

requestUserOrgs :: User -> Handler [GitHubOrg]
requestUserOrgs = maybe (pure []) requestUserNameOrgs . userGithubUsername

requestUserNameOrgs :: GitHubUserName -> Handler [GitHubOrg]
requestUserNameOrgs username = caching (githubOrgsCacheKey username) $ do
    auth <- getsYesod $ Just . OAuth . appGitHubRateLimitToken . view settingsL
    result <- liftIO $ publicOrganizationsFor' auth username

    case result of
        Left err -> [] <$ logWarnN (tshow err)
        Right orgs -> pure $ GitHubOrg <$> V.toList orgs

data MarketplaceData = MarketplaceData
    { mdPlan :: Entity MarketplacePlan
    , _mdAccount :: Entity MarketplaceAccount
    , _mdEnabledRepoIds :: [RepoId]
    }

fetchMarketplaceData
    :: MonadIO m => GitHubUserName -> SqlPersistT m (Maybe MarketplaceData)
fetchMarketplaceData login = runMaybeT $ do
    account <- fetchMarketplaceAccountForLoginT login
    plan <- getEntityT $ marketplaceAccountMarketplacePlan $ entityVal account
    fmap (MarketplaceData plan account)
        $ lift
        $ fetchMarketplaceEnabledRepoIds (entityKey plan)
        $ entityKey account

data GitHubIdentity = GitHubIdentity
    { ghiUserName :: GitHubUserName
    , ghiAvatarUrl :: Text -- ^ TODO: newtype?
    , ghiKnownRepos :: [Entity Repo]
    , ghiMarketplaceData :: Maybe MarketplaceData
    }

fetchGitHubIdentityForOrg
    :: MonadIO m => GitHubOrg -> SqlPersistT m GitHubIdentity
fetchGitHubIdentityForOrg (GitHubOrg SimpleOrganization {..}) =
    GitHubIdentity orgLogin orgAvatarUrl
        <$> fetchReposByOwnerName (nameToName orgLogin)
        <*> fetchMarketplaceData (nameToName orgLogin)
  where
    orgLogin :: GitHubUserName
    orgLogin = nameToName simpleOrganizationLogin

    orgAvatarUrl :: Text
    orgAvatarUrl = getUrl simpleOrganizationAvatarUrl

fetchGitHubIdentityForUser
    :: MonadIO m => User -> SqlPersistT m (Maybe GitHubIdentity)
fetchGitHubIdentityForUser User {..} =
    for mDetails $ \(githubId, githubUsername) ->
        GitHubIdentity githubUsername (avatarUrl githubId)
            <$> fetchReposByOwnerName (nameToName githubUsername)
            <*> fetchMarketplaceData (nameToName githubUsername)
  where
    mDetails = (,) <$> userGithubUserId <*> userGithubUsername
    avatarUrl gid =
        "https://avatars0.githubusercontent.com/u/" <> toPathPart gid <> "?v=4"

-- brittany-disable-next-binding

githubIdentityCard :: GitHubIdentity -> Widget
githubIdentityCard GitHubIdentity {..} =
    $(widgetFile "profile/github-identity-card")

-- | Wrapper for JSON instances for caching
newtype GitHubOrg = GitHubOrg SimpleOrganization

githubOrgsCacheKey :: GitHubUserName -> [Text]
githubOrgsCacheKey username = ["profile", "orgs", toPathPart username]

instance ToJSON GitHubOrg where
    toJSON (GitHubOrg SimpleOrganization{..}) = object
        [ "id" .= simpleOrganizationId
        , "login" .= simpleOrganizationLogin
        , "url" .= simpleOrganizationUrl
        , "avatarUrl" .= simpleOrganizationAvatarUrl
        ]

instance FromJSON GitHubOrg where
    parseJSON = withObject "SimpleOrganization" $ \o -> do
        simpleOrganizationId <- o .: "id"
        simpleOrganizationLogin <- o .: "login"
        simpleOrganizationUrl <- o .: "url"
        simpleOrganizationAvatarUrl <- o .: "avatarUrl"
        pure $ GitHubOrg SimpleOrganization{..}

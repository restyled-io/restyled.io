{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Profile
    ( getProfileR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.GitHubOrg
import Restyled.Models
import Restyled.PrivateRepoAllowance
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

data MarketplaceData = MarketplaceData
    { mdPlan :: Entity MarketplacePlan
    , mdAccount :: Entity MarketplaceAccount
    , mdEnabledRepoIds :: [RepoId]
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

    avatarUrl :: GitHubUserId -> Text
    avatarUrl gid =
        "https://avatars0.githubusercontent.com/u/" <> toPathPart gid <> "?v=4"

-- brittany-disable-next-binding

githubIdentityCard :: GitHubIdentity -> Widget
githubIdentityCard identity@GitHubIdentity {..} =
    $(widgetFile "profile/github-identity-card")
  where
    claimR Repo {..} = RepoP repoOwner repoName $ RepoMarketplaceP RepoMarketplaceClaimR

data MarketplaceAction
    = EnablePrivateRepo
    | DisablePrivateRepo

getMarketplaceAction :: GitHubIdentity -> Entity Repo -> Maybe MarketplaceAction
getMarketplaceAction GitHubIdentity {..} (Entity repoId Repo {..}) = do
    MarketplaceData {..} <- ghiMarketplaceData
    guard $ repoIsPrivate && isLimitedPlan mdPlan
    pure $ if repoId `notElem` mdEnabledRepoIds
        then EnablePrivateRepo
        else DisablePrivateRepo

isLimitedPlan :: Entity MarketplacePlan -> Bool
isLimitedPlan (Entity _ MarketplacePlan {..}) =
    case marketplacePlanPrivateRepoAllowance of
        PrivateRepoAllowanceNone -> False
        PrivateRepoAllowanceUnlimited -> False
        PrivateRepoAllowanceLimited _ -> True

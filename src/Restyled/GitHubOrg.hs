module Restyled.GitHubOrg
    ( GitHubOrg(..)
    , githubOrgLogin
    , requestUserOrgs
    , requestUserNameOrgs
    , githubOrgsCacheKey
    )
where

import Restyled.Prelude

import qualified Data.Vector as V
import Restyled.Cache
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

-- | Wrapper for JSON instances for caching
newtype GitHubOrg = GitHubOrg SimpleOrganization

githubOrgLogin :: GitHubOrg -> GitHubUserName
githubOrgLogin (GitHubOrg SimpleOrganization {..}) =
    nameToName simpleOrganizationLogin

githubOrgsCacheKey :: GitHubUserName -> [Text]
githubOrgsCacheKey username = ["profile", "orgs", toPathPart username]

instance ToJSON GitHubOrg where
    toJSON (GitHubOrg SimpleOrganization {..}) = object
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
        pure $ GitHubOrg SimpleOrganization { .. }

requestUserOrgs :: User -> Handler [GitHubOrg]
requestUserOrgs = maybe (pure []) requestUserNameOrgs . userGithubUsername

requestUserNameOrgs :: GitHubUserName -> Handler [GitHubOrg]
requestUserNameOrgs username = caching (githubOrgsCacheKey username) $ do
    auth <- getsYesod $ Just . OAuth . appGitHubRateLimitToken . view settingsL
    result <- liftIO $ publicOrganizationsFor' auth username

    case result of
        Left err -> [] <$ logWarnN (tshow err)
        Right orgs -> pure $ GitHubOrg <$> V.toList orgs

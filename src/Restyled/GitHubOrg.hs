module Restyled.GitHubOrg
    ( GitHubOrg(..)
    , requestUserOrgs
    , requestUserNameOrgs
    , githubOrgsCacheKey
    ) where

import Restyled.Prelude

import qualified Data.Vector as V
import Restyled.Cache
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

-- | Wrapper for JSON instances for caching
newtype GitHubOrg = GitHubOrg SimpleOrganization
    deriving newtype (Eq, Ord)

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

requestUserOrgs
    :: ( MonadIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSettings env
       )
    => User
    -> m [GitHubOrg]
requestUserOrgs = maybe (pure []) requestUserNameOrgs . userGithubUsername

requestUserNameOrgs
    :: ( MonadIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSettings env
       )
    => GitHubUserName
    -> m [GitHubOrg]
requestUserNameOrgs username = caching (githubOrgsCacheKey username) $ do
    auth <- asks $ OAuth . appGitHubRateLimitToken . view settingsL
    result <- liftIO $ githubRequest auth $ publicOrganizationsForR
        username
        FetchAll

    case result of
        Left err -> [] <$ logWarnN (tshow err)
        Right orgs -> pure $ GitHubOrg <$> V.toList orgs

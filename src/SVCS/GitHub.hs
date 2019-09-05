-- | FIXME: Rename as Restyled.GitHub?
module SVCS.GitHub
    (
    -- * Webhook payload
      GitHubPayload
    , unGitHubPayload

    -- * Installation Auth
    , githubAuthInstallation

    -- * Re-exports
    , module X
    )
where

import GitHub.Auth as X hiding (Auth)
import GitHub.Auth.JWT as X
import GitHub.Data as X (Id, getUrl, mkId, toPathPart, untagId)
import GitHub.Data.AccessTokens as X
import GitHub.Endpoints.Installations.AccessTokens as X
import GitHub.Endpoints.Organizations as X
    (SimpleOrganization(..), publicOrganizationsFor')
import GitHub.Endpoints.Repos.Collaborators.Permissions as X

import Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text.Encoding (encodeUtf8)
import GHC.Stack
import GitHub.Auth
import GitHub.Data
import GitHub.Data.Apps
import GitHub.Data.Installations
import SVCS.Names (RepoSVCS(..))
import SVCS.Payload

newtype GitHubPayload = GitHubPayload { unGitHubPayload :: Payload }

instance FromJSON GitHubPayload where
    parseJSON v@(Object o) = do
        event <- parseJSON v
        installation <- o .: "installation"

        let PullRequest {..} = pullRequestEventPullRequest event
            Repo {..} = pullRequestRepository event

        pure $ GitHubPayload Payload
            { pSVCS = GitHubSVCS
            , pAction = pullRequestEventAction event
            , pAuthor = untagName $ simpleUserLogin pullRequestUser
            , pOwnerName = simpleOwnerLogin repoOwner
            , pRepoName = repoName
            , pRepoIsPrivate = repoPrivate
            , pInstallationId = installationId installation
            , pPullRequest = pullRequestNumber
            }

    parseJSON v = typeMismatch "PullRequestEvent" v

githubAuthInstallation
    :: HasCallStack
    => Id App
    -> AppKey
    -> Id Installation
    -> IO (Either Error Auth)
githubAuthInstallation appId appKey installationId = do
    auth <- authJWTMax appId appKey
    etoken <- accessTokenFor auth installationId
    pure $ fmap (OAuth . encodeUtf8 . atToken) etoken

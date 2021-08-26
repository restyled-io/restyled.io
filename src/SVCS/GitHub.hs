-- | FIXME: Rename as Restyled.GitHub?
module SVCS.GitHub
    ( githubAuthInstallation
    , githubRequest

    -- * Re-exports
    , module X
    ) where

import GitHub.Auth as X hiding (Auth)
import GitHub.Auth.JWT as X
import GitHub.Data as X (Id, getUrl, mkId, toPathPart, untagId)
import GitHub.Data.AccessTokens as X
import GitHub.Data.Request as X (FetchCount(..))
import GitHub.Endpoints.Installations.AccessTokens as X
import GitHub.Endpoints.Organizations as X
    (SimpleOrganization(..), publicOrganizationsForR)
import GitHub.Endpoints.Repos.Collaborators.Permissions as X

import Prelude

import Data.Text.Encoding (encodeUtf8)
import GHC.Stack
import GitHub.Auth
import GitHub.Data
import GitHub.Data.Apps
import GitHub.Data.Installations
import GitHub.Request

githubAuthInstallation
    :: HasCallStack
    => Id App
    -> AppKey
    -> Id Installation
    -> IO (Either Error Auth)
githubAuthInstallation appId appKey installationId = do
    auth <- authJWTMax appId appKey
    etoken <- githubRequest auth $ accessTokenForR installationId
    pure $ fmap (OAuth . encodeUtf8 . atToken) etoken

githubRequest :: (AuthMethod am, GitHubRW req res) => am -> req -> res
githubRequest = github

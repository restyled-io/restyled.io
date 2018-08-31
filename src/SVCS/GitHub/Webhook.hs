{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SVCS.GitHub.Webhook
    ( GitHubPayload(..)
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GitHub.Data
import GitHub.Data.Apps
import SVCS.Names
import SVCS.Payload

newtype GitHubPayload = GitHubPayload Payload

instance FromJSON GitHubPayload where
    parseJSON v@(Object o) = do
        event <- parseJSON v
        installation <- o .: "installation"

        let PullRequest {..} = pullRequestEventPullRequest event
            Repo {..} = pullRequestRepository event

        pure $ GitHubPayload Payload
            { pAction = pullRequestEventAction event
            , pAuthor = untagName $ simpleUserLogin pullRequestUser
            , pOwnerName = simpleOwnerLogin repoOwner
            , pRepoName = repoName
            , pRepoIsPrivate = repoPrivate
            , pInstallationId = installationId installation
            , pPullRequest = mkPullRequestNum pullRequestNumber
            }

    parseJSON v = typeMismatch "PullRequestEvent" v

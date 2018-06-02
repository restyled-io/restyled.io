{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- TODO: upstream installation.id parsing to @"GitHub.Data.PullRequestEvent"@
--
module GitHub.Data.Webhooks.PullRequest
    ( Payload(..)
    )
where

import Prelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GitHub.Data
import GitHub.Data.Apps

data Payload = Payload
    { pAction :: PullRequestEventType
    , pPullRequest :: PullRequest
    , pRepository :: Repo
    , pInstallationId :: Id Installation
    }
    deriving Show

instance FromJSON Payload where
    parseJSON v@(Object o) = do
        event <- parseJSON v
        installation <- o .: "installation"

        pure Payload
            { pAction = pullRequestEventAction event
            , pPullRequest = pullRequestEventPullRequest event
            , pRepository = pullRequestRepository event
            , pInstallationId = installationId installation
            }

    parseJSON v = typeMismatch "PullRequestEvent" v

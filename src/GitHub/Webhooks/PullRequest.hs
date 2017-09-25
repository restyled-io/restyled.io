{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GitHub.Webhooks.PullRequest
    ( Action(..)
    , Payload(..)
    , toRepository
    , toPullRequest
    ) where

import ClassyPrelude

import Data.Aeson
import GitHub.Model

import qualified Model as M

data Action = Opened | Closed deriving (Eq, Show)

instance FromJSON Action where
    parseJSON = withText "PullRequest.Action" $ \case
        "opened" -> pure Opened
        "closed" -> pure Closed
        _ -> mzero

data Payload = Payload
    { pAction :: Action
    , pPullRequest :: PullRequest
    , pRepository :: Repository
    , pInstallationId :: GitHubId
    }
    deriving Show

instance FromJSON Payload where
    parseJSON = withObject "PullRequest.Payload" $ \o -> Payload
        <$> o .: "action"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> (o .: "installation" >>= (.: "id"))

toRepository :: Payload -> M.Repository
toRepository Payload{..} = M.Repository
    { M.repositoryFullName = rFullName pRepository
    }

toPullRequest :: M.RepositoryId -> Payload -> M.PullRequest
toPullRequest repositoryId Payload{..} = M.PullRequest
    { M.pullRequestNumber = prNumber pPullRequest
    , M.pullRequestRepository = repositoryId
    }

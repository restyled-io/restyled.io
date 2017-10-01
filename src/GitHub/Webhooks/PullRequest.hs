{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Webhooks.PullRequest
    ( Action(..)
    , Payload(..)
    ) where

import ClassyPrelude

import Data.Aeson
import GitHub.Model

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

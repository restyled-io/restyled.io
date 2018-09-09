{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module SVCS.GitLab.Webhook
    ( GitLabPayload(..)
    ) where

import Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHC.Generics
import SVCS.Names
import SVCS.Payload

data MergeRequestEvent = MergeRequestEvent
    { mreObjectAttributes :: ObjectAttributes
    , mreUser :: User
    }
    deriving Generic

instance FromJSON MergeRequestEvent where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ObjectAttributes = ObjectAttributes
    { oaState :: Text
    , oaIid :: PullRequestNum
    , oaTarget :: Target
    }
    deriving Generic

instance FromJSON ObjectAttributes where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Target = Target
    { tNamespace :: OwnerName
    , tName :: RepoName
    , tVisibilityLevel :: VisibilityLevel
    }
    deriving Generic

instance FromJSON Target where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data VisibilityLevel = Private | Internal | Public

instance FromJSON VisibilityLevel where
    parseJSON = withScientific "VisibilityLevel" $ \case
        0 -> pure Private
        10 -> pure Internal
        20 -> pure Public
        x -> fail $ "Unexpected visibility number " <> show x

newtype User = User { uUsername :: Text }
    deriving Generic

instance FromJSON User where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype GitLabPayload = GitLabPayload { unGitLabPayload :: Payload }

instance FromJSON GitLabPayload where
    parseJSON v = do
        MergeRequestEvent {..} <- parseJSON v

        let ObjectAttributes {..} = mreObjectAttributes
            Target {..} = oaTarget

        pure $ GitLabPayload Payload
            { pSVCS = GitLabSVCS
            , pAction = toPullRequestEventType oaState
            , pAuthor = uUsername mreUser
            , pOwnerName = tNamespace
            , pRepoName = tName
            , pRepoIsPrivate = toIsPrivate tVisibilityLevel
            , pInstallationId = 0
            , pPullRequest = oaIid
            }

toPullRequestEventType :: Text -> PullRequestEventType
toPullRequestEventType = \case
    "opened" -> PullRequestOpened

    -- TODO: seems to be no docs. and the ones that exist are wrong. so we'll
    -- just use something we know will be ignored.
    _ -> PullRequestAssigned

toIsPrivate :: VisibilityLevel -> Bool
toIsPrivate Public = False
toIsPrivate _ = True

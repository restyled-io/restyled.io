{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Data.Webhooks.PullRequest
    ( Action(..)
    , Payload(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import GitHub.Data
import GitHub.Data.Apps

-- | We only care about Opened so far
data Action = Opened | Other Text deriving (Eq, Show)

instance FromJSON Action where
    parseJSON = withText "PullRequest.Action" $ \case
        "opened" -> pure Opened
        t -> pure $ Other t

data Payload = Payload
    { pAction :: Action
    , pPullRequest :: PullRequest
    , pRepository :: Repo
    , pInstallationId :: Id Installation
    }
    deriving Show

instance FromJSON Payload where
    parseJSON = withObject "PullRequest.Payload" $ \o -> Payload
        <$> o .: "action"
        <*> o .: "pull_request"
        <*> o .: "repository"
        <*> (o .: "installation" >>= (.: "id"))

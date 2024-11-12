{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Restyled.Disabled
  ( emitDisabledStatus
  ) where

import Restyled.Prelude

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified GitHub
import qualified GitHub.App.Token as GitHub
import qualified GitHub.Data.Apps as GitHub
import Restyled.Settings
import UnliftIO.Exception (tryAny)

data Webhook = Webhook
  { installation :: Installation
  , pull_request :: PullRequest
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Installation = Installation
  { id :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data PullRequest = PullRequest
  { number :: Int
  , base :: Commit
  , head :: Commit
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Commit = Commit
  { repo :: Repo
  , sha :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Repo = Repo
  { private :: Bool
  , owner :: Owner
  , name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

repoFullName :: Repo -> Text
repoFullName repo = repo.owner.login <> "/" <> repo.name

newtype Owner = Owner
  { login :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

emitDisabledStatus
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasSettings env)
  => BSL.ByteString
  -> m (Either String GitHub.Status)
emitDisabledStatus body = runExceptT $ do
  webhook <- hoistEither $ eitherDecode body
  settings <- lift $ view settingsL
  token <- generateToken settings webhook

  logInfo
    $ "Emitted disabled status"
    :# [ "repository" .= repoFullName webhook.pull_request.base.repo
       , "pullRequest" .= webhook.pull_request.number
       , "commitSha" .= webhook.pull_request.head.sha
       ]

  createStatus token webhook

generateToken
  :: (MonadIO m, MonadError String m)
  => AppSettings
  -> Webhook
  -> m GitHub.AccessToken
generateToken AppSettings {..} webhook =
  unTry
    $ liftIO
    $ tryAny
    $ GitHub.generateInstallationTokenScoped create creds
    $ GitHub.InstallationId webhook.installation.id
 where
  creds =
    GitHub.AppCredentials
      { GitHub.appId = GitHub.AppId $ GitHub.untagId appGitHubAppId
      , GitHub.privateKey =
          GitHub.PrivateKey
            $ BS8.pack
            $ GitHub.unAppKey appGitHubAppKey
      }

  create = mempty {GitHub.permissions = GitHub.statuses GitHub.Write}

createStatus
  :: (MonadIO m, MonadError String m)
  => GitHub.AccessToken
  -> Webhook
  -> m GitHub.Status
createStatus token webhook =
  unTry
    $ liftIO
    $ GitHub.github (GitHub.OAuth $ encodeUtf8 token.token)
    $ GitHub.createStatusR
      (GitHub.mkOwnerName webhook.pull_request.base.repo.owner.login)
      (GitHub.mkRepoName webhook.pull_request.base.repo.name)
      (GitHub.mkCommitName webhook.pull_request.head.sha)
    $ GitHub.NewStatus
      { GitHub.newStatusState = GitHub.StatusError
      , GitHub.newStatusTargetUrl = Just $ GitHub.URL docs
      , GitHub.newStatusDescription = Just "The Restyled App has been disabled"
      , GitHub.newStatusContext = Just "restyled"
      }

docs :: Text
docs = "https://github.com/restyled-io/actions/wiki/Migrating-from-the-Restyled-App"

unTry :: (MonadError String m, Exception e) => m (Either e a) -> m a
unTry f = either (throwError . displayException) pure =<< f

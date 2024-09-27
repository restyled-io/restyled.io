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
  deriving anyclass (FromJSON)

newtype Installation = Installation
  { id :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data PullRequest = PullRequest
  { base :: Commit
  , head :: Commit
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Commit = Commit
  { repo :: Repo
  , sha :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Repo = Repo
  { owner :: Owner
  , name :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Owner = Owner
  { login :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Decide if we should emit a disabled status intead of processing
--
-- Some ideas:
--
-- - Our own testing repositories
-- - All public repositories
-- - All except those orgs who are still paying
shouldEmitDisabledStatus :: Webhook -> Bool
shouldEmitDisabledStatus webhook =
  and
    [ webhook.pull_request.base.repo.owner.login == "restyled-io"
    , webhook.pull_request.base.repo.name == "demo"
    ]

emitDisabledStatus
  :: (MonadUnliftIO m, MonadReader env m, HasSettings env)
  => BSL.ByteString
  -> m (Either String GitHub.Status)
emitDisabledStatus body = runExceptT $ do
  webhook <- hoistEither $ eitherDecode body
  guard $ shouldEmitDisabledStatus webhook
  settings <- lift $ view settingsL
  token <- generateToken settings webhook
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
docs = "https://docs.restyled.io/docs/migrating-to-github-actions/"

unTry :: (MonadError String m, Show e) => m (Either e a) -> m a
unTry f = either (throwError . show) pure =<< f

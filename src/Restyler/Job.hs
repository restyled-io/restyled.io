{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Restyler.Job
    ( Job(..)
    , newJob
    , awaitRestylerJob
    , enqueueRestylerJob
    ) where

import ClassyPrelude

import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Database.Redis hiding (decode)
import GitHub.Data
import GitHub.Data.Apps

data Job = Job
    { jId :: Id Job
    , jInstallationId :: Id Installation
    , jOwner :: Name Owner
    , jRepo :: Name Repo
    , jPullRequest :: Id PullRequest
    }
    deriving Show

deriveJSON defaultOptions ''Job

-- In IO because I assume we'll need to generate jId
newJob :: MonadIO m => Id Installation -> Repo -> PullRequest -> m Job
newJob installationId repo pullRequest = return Job
    { jId = mkId Proxy 1 -- TODO
    , jInstallationId = installationId
    , jOwner = simpleOwnerLogin $ repoOwner repo
    , jRepo = repoName repo
    , jPullRequest = mkId Proxy $ pullRequestNumber pullRequest
    }

awaitRestylerJob :: MonadIO m => Connection -> Integer -> m (Maybe Job)
awaitRestylerJob conn timeout = do
    eresult <- liftIO $ runRedis conn $ brpop [queueName] timeout
    return $ either (const Nothing) (decodePopped =<<) eresult
  where
    decodePopped = decode . fromStrict . snd

enqueueRestylerJob :: MonadIO m => Connection -> Job -> m ()
enqueueRestylerJob conn job = void $ liftIO $ runRedis conn $
    lpush queueName [toStrict $ encode job]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

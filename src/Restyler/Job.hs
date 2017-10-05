{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Restyler.Job
    ( Job(..)
    , awaitRestylerJob
    , enqueueRestylerJob
    ) where

import ClassyPrelude

import Data.Aeson
import Data.Aeson.TH
import Database.Redis hiding (decode)
import GitHub.Model

data Job = Job
    { jInstallationId :: GitHubId
    , jRepository :: Repository
    , jPullRequest :: PullRequest
    }
    deriving Show

deriveJSON defaultOptions ''Job

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

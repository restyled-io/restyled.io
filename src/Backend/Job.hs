{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend.Job
    ( insertJob
    , completeJob
    , awaitRestylerJob
    , enqueueRestylerJob
    , queueName
    ) where

import Import

import Backend.Foundation
import Data.Aeson
import Database.Persist.Sql (SqlPersistM)
import GitHub.Data
import GitHub.Data.Apps (Installation)
import System.Exit (ExitCode(..))

insertJob :: Id Installation -> Repo -> PullRequest -> YesodDB App (Entity Job)
insertJob installationId repo pullRequest = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobInstallationId = installationId
        , jobOwner = simpleOwnerLogin $ repoOwner repo
        , jobRepo = repoName repo
        , jobPullRequest = mkId Proxy $ pullRequestNumber pullRequest
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

completeJob
    :: JobId
    -> ExitCode
    -> Text -- ^ stdout
    -> Text -- ^ stderr
    -> SqlPersistM ()
completeJob jid ec out err = do
    now <- liftIO getCurrentTime
    update jid
        [ JobUpdatedAt =. now
        , JobCompletedAt =. Just now
        , JobExitCode =. Just (toInt ec)
        , JobStdout =. Just out
        , JobStderr =. Just err
        ]
  where
    toInt ExitSuccess = 0
    toInt (ExitFailure i) = i

awaitRestylerJob :: MonadBackend m => Integer -> m (Maybe (Entity Job))
awaitRestylerJob timeout = do
    $(logDebug) $ "Awaiting Restyler Job..."
    eresult <- runRedis $ brpop [queueName] timeout
    $(logDebug) $ "Popped value :" <> tshow eresult
    return $ either (const Nothing) (decodePopped =<<) eresult
  where
    decodePopped = decode . fromStrict . snd

enqueueRestylerJob :: MonadBackend m => Entity Job -> m ()
enqueueRestylerJob e@(Entity jid job) = do
    $(logDebug) $ "Enqueuing Restyler Job Id "
        <> toPathPiece jid <> " :" <> tshow job
    void $ runRedis $ lpush queueName [toStrict $ encode e]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Job
    ( insertJob
    , completeJob
    , awaitRestylerJob
    , enqueueRestylerJob
    , queueName
    ) where

import Import hiding (timeout)

import Backend.Foundation
import Data.Aeson
import Database.Persist.Sql (SqlPersistM)
import GitHub.Data hiding (Repo(..))
import System.Exit (ExitCode(..))

insertJob :: Entity Repo -> Id PullRequest -> YesodDB App (Entity Job)
insertJob (Entity _ Repo{..}) pullRequestNumber = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobInstallationId = repoInstallationId
        , jobOwner = repoOwner
        , jobRepo = repoName
        , jobPullRequest = pullRequestNumber
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
    logDebugN "Awaiting Restyler Job..."
    eresult <- runRedis $ brpop [queueName] timeout
    logDebugN $ "Popped value: " <> tshow eresult
    return $ either (const Nothing) (decodePopped =<<) eresult
  where
    decodePopped = decodeStrict . snd

enqueueRestylerJob :: MonadBackend m => Entity Job -> m ()
enqueueRestylerJob e@(Entity jid job) = do
    logDebugN $ "Enqueuing Restyler Job Id "
        <> toPathPiece jid <> ": " <> tshow job
    void $ runRedis $ lpush queueName [toStrict $ encode e]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

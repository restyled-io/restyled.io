{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Job
    ( insertJob
    , insertJobRetry
    , completeJob
    , awaitRestylerJob
    , enqueueRestylerJob
    , queueName
    ) where

import Import hiding (timeout)

import Backend.Foundation
import Data.Aeson
import System.Exit (ExitCode(..))

insertJob :: Entity Repo -> PullRequestNum -> YesodDB App (Entity Job)
insertJob (Entity _ Repo {..}) pullRequestNumber = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobSvcs = repoSvcs
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

insertJobRetry :: Job -> YesodDB App (Entity Job)
insertJobRetry job = do
    now <- liftIO getCurrentTime
    insertEntity Job
        { jobSvcs = jobSvcs job
        , jobOwner = jobOwner job
        , jobRepo = jobRepo job
        , jobPullRequest = jobPullRequest job
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

completeJob
    :: MonadIO m
    => JobId
    -> ExitCode
    -> Text -- ^ stdout
    -> Text -- ^ stderr
    -> SqlPersistT m ()
completeJob jid ec out err = do
    now <- liftIO getCurrentTime
    update
        jid
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
    where decodePopped = decodeStrict . snd

enqueueRestylerJob :: MonadBackend m => Entity Job -> m ()
enqueueRestylerJob e@(Entity jid job) = do
    logDebugN
        $ "Enqueuing Restyler Job Id "
        <> toPathPiece jid
        <> ": "
        <> tshow job
    void $ runRedis $ lpush queueName [toStrict $ encode e]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

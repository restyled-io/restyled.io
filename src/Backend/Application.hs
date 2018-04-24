{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Application
    ( backendMain
    , awaitAndProcessJob
    ) where

import Import hiding (runDB)

import Backend.DB
import Backend.Foundation
import Backend.Job
import Control.Monad ((<=<))
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Database.Redis (checkedConnect)
import GitHub.Endpoints.Installations hiding (Repo(..))
import LoadEnv (loadEnv)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..))
import System.Process (readProcessWithExitCode)

backendMain :: IO ()
backendMain = do
    loadEnv
    backendSettings <- loadEnvSettings

    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    -- In the backend, we just log to stdout; so it's simpler to repeat that
    -- knowledge here than to do the chicken-and-egg dance as in the
    -- construction of the Application connection pool.
    backendConnPool <- runStdoutLoggingT $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf backendSettings)
        (pgPoolSize $ appDatabaseConf backendSettings)

    backendRedisConn <- checkedConnect (appRedisConf backendSettings)

    runBackend Backend{..} $ forever $ awaitAndProcessJob 120

awaitAndProcessJob :: MonadBackend m => Integer -> m ()
awaitAndProcessJob = traverse_ processJob <=< awaitRestylerJob

processJob :: MonadBackend m => Entity Job -> m ()
processJob (Entity jid job) = do
    logInfoN $ "Processing Restyler Job Id "
        <> toPathPiece jid <> ": " <> tshow job
    settings <- asks backendSettings
    (ec, out, err) <-
        execRestyler settings job `catchAny` \ex -> do
            -- Log and act like a failed process
            logErrorN $ tshow ex
            pure (ExitFailure 1, "", show ex)

    runDB $ completeJob jid ec (pack out) (pack err)

execRestyler :: MonadBackend m => AppSettings -> Job -> m (ExitCode, String, String)
execRestyler appSettings@AppSettings{..} Job{..} = do
    repo <- fromMaybeM (throwString "Repo not found")
        =<< runDB (getBy $ UniqueRepo jobOwner jobRepo)

    eAccessToken <- liftIO $ createAccessToken
        appGitHubAppId
        appGitHubAppKey
        jobInstallationId

    either throwString
        (\AccessToken{..} ->
            readLoggedProcess "docker"
                [ "run", "--rm"
                , "--env", debugEnv repo
                , "--env", "GITHUB_ACCESS_TOKEN=" <> unpack atToken
                , "--volume", "/tmp:/tmp"
                , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
                , appRestylerImage ++ maybe "" (":" ++) appRestylerTag
                , unpack
                    $ toPathPiece jobOwner
                    <> "/" <> toPathPiece jobRepo
                    <> "#" <> toPathPiece jobPullRequest
                ]
        )
        eAccessToken
  where
    debugEnv (Entity _ Repo{..})
        | appSettings `allowsLevel` LevelDebug = "DEBUG=1"
        | repoDebugEnabled = "DEBUG=1"
        | otherwise = "DEBUG="

readLoggedProcess :: (MonadIO m, MonadLogger m)
    => String -> [String] -> m (ExitCode, String, String)
readLoggedProcess cmd args = do
    logDebugN $ "process: " <> tshow (cmd:args)
    result <- liftIO $ readProcessWithExitCode cmd args ""
    logDebugN $ "process result: " <> tshow result
    pure result

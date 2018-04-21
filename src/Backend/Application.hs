{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
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
import GitHub.Data (toPathPart)
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
    $(logInfo) $ "Processing Restyler Job Id "
        <> toPathPiece jid <> ": " <> tshow job
    settings <- asks backendSettings
    (ec, out, err) <- execRestyler settings job
    runDB $ completeJob jid ec (pack out) (pack err)

execRestyler :: MonadBackend m => AppSettings -> Job -> m (ExitCode, String, String)
execRestyler AppSettings{..} Job{..} = readLoggedProcess "docker"
    [ "run", "--rm"
    , "--volume", "/tmp:/tmp"
    , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
    , appRestylerImage ++ maybe "" (":" ++) appRestylerTag
    , "--github-app-id", unpack $ toPathPart appGitHubAppId
    , "--github-app-key", unpack appGitHubAppKey
    , "--installation-id", unpack $ toPathPart jobInstallationId
    , "--owner", unpack $ toPathPart jobOwner
    , "--repo", unpack $ toPathPart jobRepo
    , "--pull-request", unpack $ toPathPart jobPullRequest
    , "--restyled-root", unpack appRoot
    ]

readLoggedProcess :: (MonadIO m, MonadLogger m)
    => String -> [String] -> m (ExitCode, String, String)
readLoggedProcess cmd args = do
    $(logDebug) $ "process: " <> tshow (cmd:args)
    result <- liftIO $ readProcessWithExitCode cmd args ""
    $(logDebug) $ "process result: " <> tshow result
    pure result

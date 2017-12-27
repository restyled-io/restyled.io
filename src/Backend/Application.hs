{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
import Database.Redis (checkedConnect)
import GitHub.Data (toPathPart)
import LoadEnv (loadEnv)
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..), hSetBuffering)
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
awaitAndProcessJob timeout = traverse_ processJob =<< awaitRestylerJob timeout

processJob :: MonadBackend m => Entity Job -> m ()
processJob (Entity jid job) = do
    $(logInfo) $ "Processing Restyler Job Id "
        <> toPathPiece jid <> ": " <> tshow job
    settings <- asks backendSettings
    (ec, out, err) <- execRestyler settings job
    runDB $ completeJob jid ec (pack out) (pack err)

execRestyler :: MonadBackend m => AppSettings -> Job -> m (ExitCode, String, String)
execRestyler AppSettings{..} Job{..} = do
    let restylerImage = appRestylerImage ++ maybe "" (":" ++) appRestylerTag

    -- Always make sure we're running the latest restyler. This is wildly
    -- inefficient but we'll live with it for now until we can justify the
    -- machinery of properly tagged releases.
    (ec, out, err) <- readLoggedProcess "docker" ["pull", restylerImage]

    if ec /= ExitSuccess
        then return (ec, out, err)
        else readLoggedProcess "docker"
            [ "run", "--rm"
            , "--volume", "/tmp:/tmp"
            , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
            , restylerImage
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

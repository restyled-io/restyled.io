{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend
    ( backendMain
    , awaitAndProcessJob
    ) where

import Import

import Control.Monad.Logger
import Database.Redis (Connection, checkedConnect)
import GitHub.Data (toPathPart)
import LoadEnv (loadEnv)
import Restyler.Job
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..), hSetBuffering)
import System.Process (readProcessWithExitCode)

backendMain :: IO ()
backendMain = do
    loadEnv
    settings <- loadEnvSettings
    conn <- checkedConnect (appRedisConf settings)

    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    runStdoutLoggingT
        $ filterLogger (const (settings `allowsLevel`))
        $ forever $ awaitAndProcessJob settings conn 120

awaitAndProcessJob :: (MonadIO m, MonadLogger m) => AppSettings -> Connection -> Integer -> m ()
awaitAndProcessJob AppSettings{..} conn timeout = do
    $(logDebug) $ "Awaiting Restyler job"
    mjob <- awaitRestylerJob conn timeout

    for_ mjob $ \job@Job{..} -> do
        $(logInfo) $ "Running Restyler Job: " <> tshow job

        let
            restylerImage :: String
            restylerImage = appRestylerImage ++ maybe "" (":" ++) appRestylerTag

            restylerArguments :: [String]
            restylerArguments =
                [ "--github-app-id", unpack $ toPathPart appGitHubAppId
                , "--github-app-key", unpack appGitHubAppKey
                , "--installation-id", unpack $ toPathPart jInstallationId
                , "--owner", unpack $ toPathPart jOwner
                , "--repo", unpack $ toPathPart jRepo
                , "--pull-request", unpack $ toPathPart jPullRequest
                , "--restyled-root", unpack appRoot
                ]

        $(logDebug) $ "exec " <> tshow (restylerImage:restylerArguments)

        (ec, out, err) <- liftIO $
            readProcessWithExitCode "docker"
                (["run", "--rm", restylerImage] ++ restylerArguments) ""

        case ec of
            ExitSuccess -> $(logDebug) $ pack $ unlines
                [ "Restyler process complete"
                , "  stdout:", out
                , "  stderr:", err
                ]

            ExitFailure i -> $(logError) $ pack $ unlines
                [ "Restyler process errored (" <> show i <> "):"
                , "  stdout:", out
                , "  stderr:", err
                ]

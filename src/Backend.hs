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
import GitHub.Model
import LoadEnv (loadEnv)
import Restyler.Job
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

backendMain :: IO ()
backendMain = do
    loadEnv
    settings <- loadEnvSettings
    conn <- checkedConnect (appRedisConf settings)

    runStdoutLoggingT
        $ filterLogger (const (settings `allowsLevel`))
        $ forever $ awaitAndProcessJob settings conn 120

awaitAndProcessJob :: (MonadIO m, MonadLogger m) => AppSettings -> Connection -> Integer -> m ()
awaitAndProcessJob AppSettings{..} conn timeout = do
    $(logDebug) $ "Awaiting Restyler job"
    mjob <- awaitRestylerJob conn timeout

    for_ mjob $ \job@Job{..} -> do
        $(logInfo) $ "Running Restyler Job: " <> tshow job

        let restylerArguments =
                [ "--github-app-id", unpack $ toPathPiece appGitHubAppId
                , "--github-app-key", unpack appGitHubAppKey
                , "--installation-id", unpack $ toPathPiece jInstallationId
                , "--repository", unpack $ toPathPiece $ rFullName jRepository
                , "--pull-request", unpack $ toPathPiece $ prNumber jPullRequest
                , "--restyled-root", unpack appRoot
                ]

        (ec, out, err) <- liftIO $
            readProcessWithExitCode appRestylerExecutable restylerArguments ""

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

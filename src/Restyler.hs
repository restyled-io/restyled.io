{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Restyler
    ( runRestyler
    ) where

import Import

import GitHub.Model
import GitHub.Webhooks.PullRequest
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

runRestyler :: AppSettings -> Payload -> Handler ()
runRestyler AppSettings{..} Payload{..} = do
    let restylerFlags =
            [ "--github-app-id", unpack $ toPathPiece appGitHubAppId
            , "--github-app-key", unpack appGitHubAppKey
            , "--installation-id", unpack $ toPathPiece pInstallationId
            , "--repository", unpack $ toPathPiece $ rFullName pRepository
            , "--pull-request", unpack $ toPathPiece $ prNumber pPullRequest
            , "--restyled-root", unpack appRoot
            ]

    $(logDebug) $ tshow restylerFlags

    (ec, out, err) <- liftIO $
        -- For now we just call the process in-request. The plan is to serialize
        -- the CLI flags (or the Settings+Payload) onto a queue to a backend.
        readProcessWithExitCode appRestylerExecutable restylerFlags ""

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

    return ()

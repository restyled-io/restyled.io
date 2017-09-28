{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Restyler
    ( runRestyler
    ) where

import Import

import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

data RestylerFlags = RestylerFlags
    { rfGitHubAppId :: GitHubId
    , rfGitHubAppKey :: Text
    , rfInstallationId :: GitHubId
    , rfRepository :: RepoFullName
    , rfPullRequest :: PRNumber
    }
    deriving Show

toProcessFlags :: RestylerFlags -> [String]
toProcessFlags RestylerFlags{..} =
    [ "--github-app-id", unpack $ toPathPiece rfGitHubAppId
    , "--github-app-key", unpack rfGitHubAppKey
    , "--installation-id", unpack $ toPathPiece rfInstallationId
    , "--repository", unpack $ toPathPiece rfRepository
    , "--pull-request", unpack $ toPathPiece rfPullRequest
    ]

runRestyler :: AppSettings -> WebhookPayloadId -> Handler ()
runRestyler AppSettings{..} webhookPayloadId = do
    restylerFlags <- runDB $ do
        webhookPayload <- get404 webhookPayloadId

        RestylerFlags
            <$> pure appGitHubAppId
            <*> pure appGitHubAppKey
            <*> pure (webhookPayloadInstallationId webhookPayload)
            <*> (repositoryFullName <$> get404 (webhookPayloadRepository webhookPayload))
            <*> (pullRequestNumber <$> get404 (webhookPayloadPullRequest webhookPayload))

    $(logDebug) $ tshow restylerFlags

    (ec, out, err) <- liftIO $
        readProcessWithExitCode
        appRestylerExecutable (toProcessFlags restylerFlags) ""

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

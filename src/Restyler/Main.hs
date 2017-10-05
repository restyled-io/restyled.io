{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Restyler.Main
    ( restylerMain
    ) where

import ClassyPrelude

import GitHub.Client
import GitHub.Model
import Restyler.Clone
import Restyler.Options
import Restyler.Run
import Settings (fromTextTemplate, renderTextUrl, textFile)
import System.Exit (die)
import Yesod.Core (PathPiece(..))

restylerMain :: IO ()
restylerMain = do
    Options{..} <- parseOptions
    jwt <- encodeJWT oGitHubAppId oGitHubAppKey
    accessToken <- createAccessToken jwt oInstallationId
    pullRequest <- getPullRequest accessToken oRepoFullName oPullRequestNumber

    let bBranch = rrRef $ prBase pullRequest
        hBranch = rrRef $ prHead pullRequest
        rBranch = hBranch <> "-restyled"
        rTitle = prTitle pullRequest <> " (Restyled)"

    withinClonedRepo (remoteURL accessToken oRepoFullName) $ do
        checkoutBranch False hBranch
        either die return =<< callRestylers bBranch
        checkoutBranch True rBranch
        commitAll $ commitMessage oRestyledRoot pullRequest
        pushOrigin rBranch

    pr <- createPullRequest accessToken oRepoFullName rTitle hBranch rBranch
    void $ createComment accessToken oRepoFullName oPullRequestNumber $ commentBody oRestyledRoot pr

-- TODO: template with more details
commitMessage :: Text -> PullRequest -> Text
commitMessage _ _ = "Restyled"

commentBody :: Text -> PullRequest -> Text
commentBody root pullRequest = fromTextTemplate
    $(textFile "templates/restyled-comment.md")
    renderTextUrl

remoteURL :: AccessToken -> RepoFullName -> Text
remoteURL accessToken repoFullName = "https://x-access-token:"
    <> atToken accessToken <> "@github.com/"
    <> toPathPiece repoFullName <> ".git"

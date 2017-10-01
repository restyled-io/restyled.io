{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import ClassyPrelude

import Data.Text.Internal.Builder (toLazyText)
import GitHub.Client
import GitHub.Model
import Text.Shakespeare.Text (textFile, renderTextUrl)
import Yesod.Core (PathPiece(..))

import Restyler.Clone
import Restyler.Options
import Restyler.Formatters

main :: IO ()
main = do
    Options{..} <- parseOptions
    jwt <- encodeJWT oGitHubAppId oGitHubAppKey
    accessToken <- createAccessToken jwt oInstallationId
    pullRequest <- getPullRequest accessToken oRepoFullName oPullRequestNumber

    -- N.B. our Base is the source PR's Head
    let branch = rrRef $ prBase pullRequest
        bBranch = rrRef $ prHead pullRequest
        hBranch = bBranch <> "-restyled"
        restyledPRTitle = prTitle pullRequest <> " (Restyled)"

    withinClonedRepo (remoteURL accessToken oRepoFullName) $ do
        checkoutBranch False bBranch
        checkoutBranch True hBranch

        runFormatters =<< changedPaths branch

        commitAll "Restyled"
        pushOrigin hBranch

    pr <- createPullRequest accessToken oRepoFullName restyledPRTitle bBranch hBranch
    void $ createComment accessToken oRepoFullName oPullRequestNumber $ commentBody oRestyledRoot pr

commentBody :: Text -> PullRequest -> Text
commentBody root pullRequest = toStrict $ toLazyText
    $ $(textFile "templates/restyled-comment.md") renderTextUrl

remoteURL :: AccessToken -> RepoFullName -> Text
remoteURL accessToken repoFullName = "https://x-access-token:"
    <> atToken accessToken <> "@github.com/"
    <> toPathPiece repoFullName <> ".git"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import ClassyPrelude

import GitHub.Client
import GitHub.Model
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
commentBody root pullRequest = unlines
    [ "Hi there!"
    , ""
    , "I just wanted to let you know that some code in this PR might not match"
        <> " the team's preferred styles. This process isn't perfect, but when"
        <> " we ran some auto-reformatting tools on it there were differences."
        <> " Those differences can be seen in #"
        <> toPathPiece (prNumber pullRequest) <> "."
        <> " To incorporate the changes, merge that PR into yours."
    , ""
    , "Sorry if this was unexpected. To disable it, see our"
        <> " [documentation](" <> root <> "/docs#disable)."
    , ""
    , "Thanks,"
    , "[Restyled.io](" <> root <> ")"
    ]

remoteURL :: AccessToken -> RepoFullName -> Text
remoteURL accessToken repoFullName = "https://x-access-token:"
    <> atToken accessToken <> "@github.com/"
    <> toPathPiece repoFullName <> ".git"

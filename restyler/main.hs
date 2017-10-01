{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
--
-- Example:
--
-- > stack exec restyler -- \
-- >   --github-app-id 5355 \
-- >   --github-app-key "$(cat ~/downloads/restyled-io-development.2017-09-19.private-key.pem)" \
-- >   --installation-id 54123 \
-- >   --repository restyled-io/demo \
-- >   --pull-request 1
--
-- 1. Clone the repo, authenticating as a GitHub App
-- 2. Checkout the head branch of the PR
-- 3. Checkout a -restyled branch
-- 4. "Restyle" the code
-- 5. Commit and push
-- 6. Open PR with -restyled branch
-- 7. Comment on original PR, linking to restyled PR
--
module Main (main) where

import ClassyPrelude

import GitHub.Client
import GitHub.Model
import System.Process (callProcess, readProcess)
import Yesod.Core (PathPiece(..))

import Data.Text.Internal.Builder (toLazyText)
--import Data.Text.Lazy (toStrict)
import Text.Shakespeare.Text (renderTextUrl, textFile)

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
        branchName = unpack $ unBranch branch
        bBranchName = unpack $ unBranch bBranch
        hBranchName = unpack $ unBranch hBranch
        restyledPRTitle = prTitle pullRequest <> " (Restyled)"

    wasRestyled <- withinClonedRepo accessToken oRepoFullName $ do
        -- Is there a Haskell libgit2 binding?
        callProcess "git" ["checkout", bBranchName]
        callProcess "git" ["checkout", "-b", hBranchName]

        changedPaths <- lines
            <$> readProcess "git" ["diff", "--name-only", branchName] ""

        putStrLn $ "Running formatters on " <> tshow changedPaths
        runFormatters changedPaths

        -- N.B. this fails if there's nothing to commit
        callProcess "git" ["commit", "-am", "Restyled"]
        callProcess "git" ["push", "-u", "origin", hBranchName]
        return True

    when wasRestyled $ do
        pr <- createPullRequest accessToken oRepoFullName restyledPRTitle bBranch hBranch
        void $ createComment accessToken oRepoFullName oPullRequestNumber $ commentBody oRestyledRoot pr

        putStrLn $ "Restyled Pull Request: https://github.com/"
            <> toPathPiece oRepoFullName <> "/pull/"
            <> toPathPiece (prNumber pr)

commentBody :: Text -> PullRequest -> Text
commentBody root pullRequest = toStrict $ toLazyText
    $ $(textFile "templates/restyled-comment.md") renderTextUrl

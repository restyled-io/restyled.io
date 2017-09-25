{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import System.Process (callProcess)
import Yesod.Core (PathPiece(..))

import Restyler.Clone
import Restyler.Options

main :: IO ()
main = do
    Options{..} <- parseOptions
    jwt <- encodeJWT oGitHubAppId oGitHubAppKey
    accessToken <- createAccessToken jwt oInstallationId
    pullRequest <- getPullRequest accessToken oRepoFullName oPullRequestNumber

    -- N.B. our Base is the source PR's Head
    let bBranch = rrRef $ prHead pullRequest
        hBranch = bBranch <> "-restyled"
        bBranchName = unpack $ unBranch bBranch
        hBranchName = unpack $ unBranch hBranch
        restyledPRTitle = prTitle pullRequest <> " (Restyled)"

    wasRestyled <- withinClonedRepo accessToken oRepoFullName $ do
        callProcess "git" ["checkout", bBranchName]
        callProcess "git" ["checkout", "-b", hBranchName]

        -- For now, the Null Restyler
        callProcess "sh" ["-c", "printf \"%s\\n\" \"\" \"zomg restyled\" >> Demo.hs"]

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

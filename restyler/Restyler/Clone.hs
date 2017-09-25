{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Restyler.Clone
    ( withinClonedRepo
    ) where

import ClassyPrelude

import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Yesod.Core (PathPiece(..))

import GitHub.Model

withinClonedRepo :: AccessToken -> RepoFullName -> IO a -> IO a
withinClonedRepo accessToken repoFullName act = withinTemporaryDirectory $ do
    callProcess "git" ["clone", unpack $ remoteURL accessToken repoFullName, "repo"]
    withCurrentDirectory "repo" act

withinTemporaryDirectory :: IO a -> IO a
withinTemporaryDirectory act = withSystemTempDirectory "" $ \dir -> do
    withCurrentDirectory dir act

remoteURL :: AccessToken -> RepoFullName -> Text
remoteURL accessToken repoFullName = "https://x-access-token:"
    <> atToken accessToken <> "@github.com/"
    <> toPathPiece repoFullName <> ".git"

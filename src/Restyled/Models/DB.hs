{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

-- | Entities defined via @config/models@
module Restyled.Models.DB
    ( module Restyled.Models.DB
    )
where

import Restyled.Prelude

import Database.Persist.Quasi
import Database.Persist.TH
import Restyled.PrivateRepoAllowance

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

instance Display (Entity User) where
    display (Entity userId User {..}) =
        "User #"
            <> display (toPathPiece userId)
            <> " "
            <> maybe "<GitHub unknown>" display userGithubUsername

instance Display (Entity Repo) where
    display (Entity repoId Repo {..}) =
        "Repo #"
            <> display (toPathPiece repoId)
            <> " "
            <> display repoOwner
            <> "/"
            <> display repoName

instance Display (Entity Job) where
    display (Entity jobId Job {..}) =
        "Job #"
            <> display (toPathPiece jobId)
            <> " for "
            <> display jobOwner
            <> "/"
            <> display jobRepo
            <> "#"
            <> display jobPullRequest

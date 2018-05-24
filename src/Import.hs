{-# LANGUAGE RecordWildCards #-}

module Import
    ( module Import
    ) where

import Foundation as Import
import Import.NoFoundation as Import

repoJobsRoute :: Entity Repo -> Route App
repoJobsRoute (Entity _ Repo {..}) =
    OwnerP repoOwner $ ReposP $ RepoP repoName $ RepoJobsP RepoJobsR

repoJobRoute :: Entity Job -> Route App
repoJobRoute (Entity jobId Job {..}) =
    OwnerP jobOwner $ ReposP $ RepoP jobRepo $ RepoJobsP $ RepoJobR jobId

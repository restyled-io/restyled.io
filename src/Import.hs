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

-- | N.B. Supports only @PATCH@
adminRepoRoute :: Entity Repo -> Route App
adminRepoRoute (Entity repoId _) =
    AdminP $ AdminReposP $ AdminRepoP repoId AdminRepoR

adminRepoJobsRoute :: Entity Repo -> Route App
adminRepoJobsRoute (Entity repoId _) =
    AdminP $ AdminReposP $ AdminRepoP repoId AdminRepoJobsR

-- | N.B. Supports only @POST@
adminJobsRoute :: Route App
adminJobsRoute = AdminP $ AdminJobsP AdminJobsR

-- | N.B. Supports only @DELETE@
adminJobRoute :: Entity Job -> Route App
adminJobRoute (Entity jobId _) =
    AdminP $ AdminJobsP $ AdminJobR jobId

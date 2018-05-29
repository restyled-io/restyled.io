{-# LANGUAGE RecordWildCards #-}

module Import
    ( module Import
    )
where

import Foundation as Import
import GitHub.Data (untagId)
import Import.NoFoundation as Import

repoJobsRoute :: Entity Repo -> Route App
repoJobsRoute (Entity _ Repo {..}) =
    OwnerP repoOwner $ ReposP $ RepoP repoName $ RepoJobsP RepoJobsR

repoJobRoute :: Entity Job -> Route App
repoJobRoute (Entity jobId Job {..}) =
    OwnerP jobOwner $ ReposP $ RepoP jobRepo $ RepoJobsP $ RepoJobR jobId

jobOwnerReposRoute :: Job -> Route App
jobOwnerReposRoute Job {..} = OwnerP jobOwner $ ReposP ReposR

jobRepoJobsRoute :: Job -> Route App
jobRepoJobsRoute Job {..} =
    OwnerP jobOwner $ ReposP $ RepoP jobRepo $ RepoJobsP RepoJobsR

jobRepoPullJobsRoute :: Job -> Route App
jobRepoPullJobsRoute Job {..} =
    OwnerP jobOwner
        $ ReposP
        $ RepoP jobRepo
        $ RepoPullsP
        $ RepoPullP (untagId jobPullRequest)
        $ RepoPullJobsP RepoPullJobsR

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
adminJobRoute (Entity jobId _) = AdminP $ AdminJobsP $ AdminJobR jobId

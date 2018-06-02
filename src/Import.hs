{-# LANGUAGE RecordWildCards #-}

module Import
    ( module Import
    )
where

import Foundation as Import
import Import.NoFoundation as Import

import qualified GitHub.Data as GH

ownerRepoP :: GH.Name GH.Owner -> GH.Name GH.Repo -> RepoP -> Route App
ownerRepoP owner name = OwnerP owner . ReposP . RepoP name

jobsR :: RepoP
jobsR = RepoJobsP RepoJobsR

jobR :: JobId -> RepoP
jobR = RepoJobsP . RepoJobR

pullJobsR :: GH.Id GH.PullRequest -> RepoP
pullJobsR num = RepoPullsP
    $ RepoPullP (GH.untagId num)
    $ RepoPullJobsP RepoPullJobsR

repoJobsRoute :: Entity Repo -> Route App
repoJobsRoute (Entity _ Repo {..}) = ownerRepoP repoOwner repoName jobsR

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

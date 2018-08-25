-- | Route helpers
module Routes
    (
    -- * Repos
      repoP
    , repoR

    -- * Repos' Pulls
    , pullP
    , pullR

    -- * Repos' Jobs
    , jobsR
    , jobR

    -- * Repos' Pulls' Jobs
    , pullJobsR

    -- * Admin routes
    , adminReposP
    , adminRepoP
    , adminRepoR
    , adminJobsP
    )
where

import Import.NoFoundation

import Foundation

repoP :: OwnerName -> RepoName -> RepoP -> Route App
repoP = RepoP

repoR :: OwnerName -> RepoName -> Route App
repoR owner name = repoP owner name RepoR

pullP :: PullRequestId -> RepoPullP -> RepoP
pullP = RepoPullP

pullR :: PullRequestId -> RepoP
pullR num = pullP num RepoPullR

jobsR :: RepoP
jobsR = RepoJobsP RepoJobsR

jobR :: JobId -> RepoP
jobR = RepoJobsP . RepoJobR

pullJobsR :: PullRequestId -> RepoP
pullJobsR num = pullP num $ RepoPullJobsP RepoPullJobsR

adminReposP :: AdminReposP -> Route App
adminReposP = AdminP . AdminReposP

adminRepoP :: RepoId -> AdminRepoP -> Route App
adminRepoP repoId = adminReposP . AdminRepoP repoId

adminRepoR :: RepoId -> Route App
adminRepoR repoId = adminRepoP repoId AdminRepoR

adminJobsP :: AdminJobsP -> Route App
adminJobsP = AdminP . AdminJobsP

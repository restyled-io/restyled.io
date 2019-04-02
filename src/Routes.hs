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
    , adminJobsP
    )
where

import Import.NoFoundation

import Foundation

repoP :: OwnerName -> RepoName -> RepoP -> Route App
repoP = RepoP

repoR :: OwnerName -> RepoName -> Route App
repoR owner name = repoP owner name RepoR

pullP :: PullRequestNum -> RepoPullP -> RepoP
pullP = RepoPullP

pullR :: PullRequestNum -> RepoP
pullR num = pullP num RepoPullR

jobsR :: RepoP
jobsR = RepoJobsP RepoJobsR

jobR :: JobId -> RepoP
jobR = RepoJobsP . RepoJobR

pullJobsR :: PullRequestNum -> RepoP
pullJobsR num = pullP num $ RepoPullJobsP RepoPullJobsR

adminReposP :: AdminReposP -> Route App
adminReposP = AdminP . AdminReposP

adminRepoP :: RepoId -> AdminRepoP -> Route App
adminRepoP repoId = adminReposP . AdminRepoP repoId

adminJobsP :: AdminJobsP -> Route App
adminJobsP = AdminP . AdminJobsP

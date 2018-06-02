-- | Route helpers
module Routes
    (
    -- * Repos
      reposP
    , reposR
    , repoP
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
import qualified GitHub.Data as GH

reposP :: GH.Name GH.Owner -> ReposP -> Route App
reposP owner = OwnerP owner . ReposP

reposR :: GH.Name GH.Owner -> Route App
reposR owner = reposP owner ReposR

repoP :: GH.Name GH.Owner -> GH.Name GH.Repo -> RepoP -> Route App
repoP owner name = reposP owner . RepoP name

repoR :: GH.Name GH.Owner -> GH.Name GH.Repo -> Route App
repoR owner name = repoP owner name RepoR

pullP :: GH.Id GH.PullRequest -> RepoPullP -> RepoP
pullP num = RepoPullsP . RepoPullP (GH.untagId num)

pullR :: GH.Id GH.PullRequest -> RepoP
pullR num = pullP num RepoPullR

jobsR :: RepoP
jobsR = RepoJobsP RepoJobsR

jobR :: JobId -> RepoP
jobR = RepoJobsP . RepoJobR

pullJobsR :: GH.Id GH.PullRequest -> RepoP
pullJobsR num = pullP num $ RepoPullJobsP RepoPullJobsR

adminReposP :: AdminReposP -> Route App
adminReposP = AdminP . AdminReposP

adminRepoP :: RepoId -> AdminRepoP -> Route App
adminRepoP repoId = adminReposP . AdminRepoP repoId

adminRepoR :: RepoId -> Route App
adminRepoR repoId = adminRepoP repoId AdminRepoR

adminJobsP :: AdminJobsP -> Route App
adminJobsP = AdminP . AdminJobsP

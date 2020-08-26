-- | Route helpers
module Restyled.Routes
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
    , jobLogR

    -- * Repos' Pulls' Jobs
    , pullJobsR

    -- * Admin
    , adminStatsReposR
    , adminStatsJobsR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.Yesod

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
jobR jobId = RepoJobsP $ RepoJobP jobId RepoJobR

jobLogR :: JobId -> RepoP
jobLogR jobId = RepoJobsP $ RepoJobP jobId RepoJobLogLinesR

pullJobsR :: PullRequestNum -> RepoP
pullJobsR num = pullP num $ RepoPullJobsP RepoPullJobsR

adminStatsReposR :: Route App
adminStatsReposR = AdminP $ AdminStatsP $ AdminStatsReposP AdminStatsReposR

adminStatsJobsR :: Route App
adminStatsJobsR = AdminP $ AdminStatsP $ AdminStatsJobsP AdminStatsJobsR

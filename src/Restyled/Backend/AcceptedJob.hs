{-# LANGUAGE LambdaCase #-}

-- | Type and handler for Jobs that we accept or ignored
module Restyled.Backend.AcceptedJob
    ( AcceptedJob(..)
    , acceptJob
    , IgnoredJobReason(..)
    , ignoredJobReasonToLogMessage
    , ignoredJobReasonToJobLogLine
    )
where

import Restyled.Prelude

import Restyled.Backend.AcceptedWebhook
import Restyled.Backend.ConcurrentJobs
import Restyled.Backend.Marketplace
import Restyled.Models

data IgnoredJobReason
    = NonGitHubRepo Repo
    | ManualRestriction Repo
    | PlanLimitation Repo MarketplacePlanLimitation
    | NewerJobInProgress (Entity Job)

data AcceptedJob = AcceptedJob
    { ajRepo :: Entity Repo
    , ajJob :: Entity Job
    , ajStaleJobs :: StaleJobs
    }

-- | Given an Accepted webhook, accept or ignore that Job
acceptJob
    :: (MonadUnliftIO m, MonadReader env m, HasDB env)
    => AcceptedWebhook
    -> ExceptT IgnoredJobReason m AcceptedJob
acceptJob AcceptedWebhook {..} = do
    when (repoSvcs repo /= GitHubSVCS) $ throwError $ NonGitHubRepo repo
    unless (repoEnabled repo) $ throwError $ ManualRestriction repo
    whenMarketplacePlanForbids allows $ throwError . PlanLimitation repo
    staleJobs <- checkConcurrentJobs awJob NewerJobInProgress
    pure $ AcceptedJob awRepo awJob staleJobs
  where
    repo = entityVal awRepo
    allows = awMarketplaceAllows

ignoredJobReasonToLogMessage :: IgnoredJobReason -> String
ignoredJobReasonToLogMessage = \case
    NonGitHubRepo _ -> "Non-GitHub repository"
    ManualRestriction _ -> "Manual restriction"
    PlanLimitation _ MarketplacePlanNotFound -> "No Marketplace Plan"
    PlanLimitation _ MarketplacePlanPublicOnly ->
        "Public-only Marketplace Plan"
    PlanLimitation _ MarketplacePlanMaxRepos -> "Maximum private repos in use"
    NewerJobInProgress _ -> "Newer Job in progress"

ignoredJobReasonToJobLogLine :: IgnoredJobReason -> String
ignoredJobReasonToJobLogLine = unlines . \case
    NonGitHubRepo repo ->
        [ "Non-GitHub (" <> show (repoSvcs repo) <> "): " <> path repo <> "."
        , "See https://github.com/restyled-io/restyled.io/issues/76"
        ]
    ManualRestriction repo ->
        [ "Repository (" <> path repo <> ") was temporarily disabled."
        , "Please contact support@restyled.com for more information."
        ]
    PlanLimitation repo MarketplacePlanNotFound ->
        [ "No active plan for private repository: " <> path repo <> "."
        , "Purchase a plan at https://github.com/marketplace/restyled-io"
        ]
    PlanLimitation _ MarketplacePlanPublicOnly ->
        [ "Your plan does not allow private repositories."
        , "Upgrade your plan at https://github.com/marketplace/restyled-io"
        ]
    PlanLimitation _ MarketplacePlanMaxRepos ->
        [ "You've reached the limit for private repositories on this plan."
        , "Upgrade your plan at https://github.com/marketplace/restyled-io"
        ]
    NewerJobInProgress (Entity jobId job) ->
        [ "Newer Job #" <> unpack (toPathPiece jobId) <> " already in progress."
        , "  Created at " <> show (jobCreatedAt job)
        ]
  where
    path :: Repo -> String
    path Repo {..} = unpack $ repoPath repoOwner repoName

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
import Restyled.Backend.Marketplace
import Restyled.Models

data IgnoredJobReason
    = NonGitHubRepo Repo
    | ManualRestriction Repo
    | PlanLimitation Repo MarketplacePlanLimitation

data AcceptedJob = AcceptedJob
    { ajRepo :: Entity Repo
    , ajJob :: Entity Job
    }

-- | Given an Accepted webhook, accept or ignore that Job
acceptJob
    :: MonadIO m
    => [Text]
    -> AcceptedWebhook
    -> ExceptT IgnoredJobReason m AcceptedJob
acceptJob restrictedRepos AcceptedWebhook {..} = do
    when (repoSvcs repo /= GitHubSVCS) $ throwError $ NonGitHubRepo repo
    when restricted $ throwError $ ManualRestriction repo
    whenMarketplacePlanForbids allows $ throwError . PlanLimitation repo
    pure $ AcceptedJob awRepo awJob
  where
    repo = entityVal awRepo
    allows = awMarketplaceAllows
    restricted =
        repoPath (repoOwner repo) (repoName repo) `elem` restrictedRepos

ignoredJobReasonToLogMessage :: IgnoredJobReason -> String
ignoredJobReasonToLogMessage = \case
    NonGitHubRepo _ -> "Non-GitHub repository"
    ManualRestriction _ -> "Manual restriction"
    PlanLimitation _ MarketplacePlanNotFound -> "No Marketplace Plan"
    PlanLimitation _ MarketplacePlanPublicOnly ->
        "Public-only Marketplace Plan"
    PlanLimitation _ MarketplacePlanMaxRepos -> "Maximum private repos in use"

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
  where
    path :: Repo -> String
    path Repo {..} = unpack $ repoPath repoOwner repoName

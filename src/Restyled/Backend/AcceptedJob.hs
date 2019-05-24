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
    | PlanLimitation Repo MarketplacePlanLimitation

data AcceptedJob = AcceptedJob
    { ajRepo :: Entity Repo
    , ajJob :: Entity Job
    }

-- | Given an Accepted webhook, accept or ignore that Job
acceptJob
    :: MonadIO m => AcceptedWebhook -> ExceptT IgnoredJobReason m AcceptedJob
acceptJob AcceptedWebhook {..} = do
    when (repoSvcs repo /= GitHubSVCS) $ throwError $ NonGitHubRepo repo
    whenMarketplacePlanForbids allows $ throwError . PlanLimitation repo
    pure $ AcceptedJob awRepo awJob
  where
    repo = entityVal awRepo
    allows = awMarketplaceAllows

ignoredJobReasonToLogMessage :: IgnoredJobReason -> String
ignoredJobReasonToLogMessage = \case
    NonGitHubRepo _ -> "Non-GitHub repository"
    PlanLimitation _ MarketplacePlanNotFound -> "No Marketplace Plan"
    PlanLimitation _ MarketplacePlanPublicOnly ->
        "Public-only Marketplace Plan"

ignoredJobReasonToJobLogLine :: IgnoredJobReason -> String
ignoredJobReasonToJobLogLine = unlines . \case
    NonGitHubRepo repo ->
        [ "Non-GitHub (" <> show (repoSvcs repo) <> "): " <> path repo <> "."
        , "See https://github.com/restyled-io/restyled.io/issues/76"
        ]
    PlanLimitation repo MarketplacePlanNotFound ->
        [ "No active plan for private repository: " <> path repo <> "."
        , "Contact support@restyled.io if you would like to discuss a Trial"
        ]
    PlanLimitation _ MarketplacePlanPublicOnly ->
        [ "Your plan does not allow private repositories."
        , "Contact support@restyled.io if you would like to discuss a Trial"
        ]
  where
    path :: Repo -> String
    path Repo {..} = unpack $ repoPath repoOwner repoName

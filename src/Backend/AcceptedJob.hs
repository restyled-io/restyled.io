{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Type and handler for Jobs that we accept or ignored
module Backend.AcceptedJob
    ( AcceptedJob(..)
    , acceptJob
    , IgnoredJob(..)
    )
where

import Import.NoFoundation

import Backend.AcceptedWebhook
import Backend.Marketplace
import Control.Monad.Except

data AcceptedJob = AcceptedJob
    { ajRepo :: Entity Repo
    , ajJob :: Entity Job
    }

newtype IgnoredJob = IgnoredJob
    { unIgnoredJob :: Entity Job
    }

-- | Given an Accepted webhook, accept or ignore that Job
acceptJob :: MonadIO m => AcceptedWebhook -> ExceptT IgnoredJob m AcceptedJob
acceptJob AcceptedWebhook {..} = do
    when (repoSvcs repo /= GitHubSVCS) $ ignoreJob awJob $ NonGitHubRepo repo
    whenMarketplacePlanForbids allows $ ignoreJob awJob . PlanLimitation repo
    pure $ AcceptedJob awRepo awJob
  where
    repo = entityVal awRepo
    allows = awMarketplaceAllows

-- | Ignore the given @'Job'@ with reason
ignoreJob
    :: MonadIO m => Entity Job -> IgnoredJobReason -> ExceptT IgnoredJob m a
ignoreJob job reason = do
    now <- liftIO getCurrentTime
    throwError
        $ IgnoredJob
        $ overEntity job
        $ completeJobSkipped now
        $ toIgnoredJobStdout reason

data IgnoredJobReason
    = NonGitHubRepo Repo
    | PlanLimitation Repo MarketplacePlanLimitation

toIgnoredJobStdout :: IgnoredJobReason -> String
toIgnoredJobStdout = unlines . \case
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

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
import qualified Data.Text as T

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
    when (repoSvcs repo /= GitHubSVCS) $ ignoreJob $ NonGitHubRepo repo
    whenMarketplacePlanForbids allows $ ignoreJob . PlanLimitation repo
    pure $ AcceptedJob awRepo awJob
  where
    repo = entityVal awRepo
    allows = awMarketplaceAllows
    ignoreJob = throwError <=< toIgnoredJob awJob

data IgnoredJobReason
    = NonGitHubRepo Repo
    | PlanLimitation Repo MarketplacePlanLimitation

toIgnoredJob :: MonadIO m => Entity Job -> IgnoredJobReason -> m IgnoredJob
toIgnoredJob (Entity jobId job) reason = do
    now <- liftIO getCurrentTime

    pure $ IgnoredJob $ Entity
        jobId
        job
            { jobUpdatedAt = now
            , jobCompletedAt = Just now
            , jobExitCode = Just 0
            , jobStdout = Just $ toIgnoredJobStdout reason
            , jobStderr = Just ""
            }

toIgnoredJobStdout :: IgnoredJobReason -> Text
toIgnoredJobStdout = T.unlines . \case
    NonGitHubRepo repo ->
        [ "Non-GitHub (" <> tshow (repoSvcs repo) <> "): " <> path repo <> "."
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
    path :: Repo -> Text
    path Repo {..} = repoPath repoOwner repoName

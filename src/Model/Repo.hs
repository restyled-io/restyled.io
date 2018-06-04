{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Repo
    ( RepoWithStats(..)
    , repoWithStats
    , IgnoredWebhookReason(..)
    , initializeFromWebhook
    )
where

import ClassyPrelude

import Database.Persist
import GitHub.Data hiding (Repo(..), User(..))
import qualified GitHub.Data as GH
import GitHub.Data.Apps hiding (installationId)
import GitHub.Data.Webhooks.PullRequest
import Model

data RepoWithStats = RepoWithStats
    { rwsRepo :: Entity Repo
    , rwsJobCount :: Int
    , rwsErrorCount :: Int
    , rwsLastJob :: Maybe (Entity Job)
    }

repoWithStats :: Entity Repo -> DB RepoWithStats
repoWithStats repo =
    RepoWithStats repo
        <$> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                ]
        <*> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                , JobExitCode !=. Just 0
                , JobExitCode !=. Nothing
                ]
        <*> selectFirst
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                , JobCompletedAt !=. Nothing
                ]
                [Desc JobCreatedAt]

data IgnoredWebhookReason
    = IgnoredAction PullRequestEventType
    | OwnPullRequest Text
    | PrivateNoPlan (Name Owner) (Name GH.Repo)

initializeFromWebhook
    :: Payload -> DB (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook Payload {..}
    | pAction `notElem` enqueueEvents
    = pure $ Left $ IgnoredAction pAction
    | "-restyled" `isSuffixOf` headBranch pPullRequest
    = pure $ Left $ OwnPullRequest $ headBranch pPullRequest
    | repoPublic pRepository
    = Right <$> findOrCreateRepo pRepository pInstallationId
    | otherwise
    = do
        now <- liftIO getCurrentTime
        repo <- findOrCreateRepo pRepository pInstallationId
        mPlan <- selectActivePlan now $ entityVal repo
        pure $ maybe
            (Left $ PrivateNoPlan
                (repoOwner $ entityVal repo)
                (repoName $ entityVal repo)
            )
            (const $ Right repo)
            mPlan

selectActivePlan :: UTCTime -> Repo -> DB (Maybe (Entity Plan))
selectActivePlan now repo = selectFirst filters [Desc PlanId]
  where
    filters = repoFilters <> activeFilters <> expiredFilters
    repoFilters = [PlanOwner ==. repoOwner repo, PlanRepo ==. repoName repo]
    activeFilters = [PlanActiveAt ==. Nothing] ||. [PlanActiveAt <=. Just now]
    expiredFilters =
        [PlanExpiresAt ==. Nothing] ||. [PlanExpiresAt >=. Just now]

findOrCreateRepo :: GH.Repo -> Id Installation -> DB (Entity Repo)
findOrCreateRepo ghRepo installationId = do
    let
        repo = Repo
            { repoOwner = simpleOwnerLogin $ GH.repoOwner ghRepo
            , repoName = GH.repoName ghRepo
            , repoInstallationId = installationId
            , repoIsPrivate = GH.repoPrivate ghRepo
            , repoDebugEnabled = False
            }

    upsert
        repo
        [ RepoInstallationId =. repoInstallationId repo
        , RepoIsPrivate =. repoIsPrivate repo
        ]

enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized]

repoPublic :: GH.Repo -> Bool
repoPublic = not . GH.repoPrivate

headBranch :: PullRequest -> Text
headBranch = pullRequestCommitRef . pullRequestHead

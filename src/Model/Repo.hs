{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Repo
    ( RepoWithStats(..)
    , repoWithStats
    , initializeFromWebhook
    )
where

import ClassyPrelude

import Control.Monad.Logger
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

-- brittany-disable-next-binding
initializeFromWebhook :: Payload -> DB (Maybe (Entity Repo))
initializeFromWebhook Payload {..}
    | pAction `notElem` enqueueEvents = pure Nothing
    | "-restyled" `isSuffixOf` headBranch pPullRequest = pure Nothing
    | repoPublic pRepository =
        Just <$> findOrCreateRepo pRepository pInstallationId
    | otherwise = do
        now <- liftIO getCurrentTime
        repo <- findOrCreateRepo pRepository pInstallationId
        mPlan <- selectActivePlan now $ entityVal repo

        for mPlan $ \(Entity _ plan) -> do
            lift
                $ logInfoN
                $ "Private repository with "
                <> tshow (planType plan)
                <> " plan"

            pure repo

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

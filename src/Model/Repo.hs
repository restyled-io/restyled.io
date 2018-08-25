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
    | IgnoredEventType Text
    | OwnPullRequest Text
    | PrivateNoPlan (Name Owner) (Name GH.Repo)

initializeFromWebhook
    :: Payload -> DB (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook Payload {..}
    | pAction `notElem` enqueueEvents
    = pure $ Left $ IgnoredAction pAction
    | "-restyled" `isSuffixOf` headBranch pPullRequest
    = pure $ Left $ OwnPullRequest $ headBranch pPullRequest
    | otherwise
    = Right <$> findOrCreateRepo pRepository pInstallationId

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

headBranch :: PullRequest -> Text
headBranch = pullRequestCommitRef . pullRequestHead

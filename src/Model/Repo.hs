{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Repo
    ( RepoWithStats(..)
    , repoWithStats
    , IgnoredWebhookReason(..)
    , initializeFromWebhook
    )
where

import ClassyPrelude

import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import qualified GitHub.Data as GH
import GitHub.Data.PullRequests
import GitHub.Data.Webhooks.PullRequest
import Model

data RepoWithStats = RepoWithStats
    { rwsRepo :: Entity Repo
    , rwsJobCount :: Int
    , rwsErrorCount :: Int
    , rwsLastJob :: Maybe (Entity Job)
    }

repoWithStats :: MonadIO m => Entity Repo -> SqlPersistT m RepoWithStats
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
    | OwnPullRequest Text Text
    | PrivateNoPlan OwnerName RepoName

initializeFromWebhook
    :: MonadIO m
    => Payload
    -> SqlPersistT m (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook Payload {..}
    | pAction `notElem` enqueueEvents = pure $ Left $ IgnoredAction pAction
    | isRestyled pPullRequest = pure $ Left $ OwnPullRequest
        (simpleAuthor pPullRequest)
        (headBranch pPullRequest)
    | otherwise = Right <$> findOrCreateRepo pRepository pInstallationId

findOrCreateRepo
    :: MonadIO m => GH.Repo -> InstallationId -> SqlPersistT m (Entity Repo)
findOrCreateRepo ghRepo installationId = do
    let
        repo = Repo
            { repoOwner = GH.simpleOwnerLogin $ GH.repoOwner ghRepo
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

isRestyled :: PullRequest -> Bool
isRestyled pr = isRestyledAuthor pr && isRestyledBranch pr
  where
    isRestyledAuthor = not . isActualAuthor . simpleAuthor
    isRestyledBranch = ("-restyled" `isSuffixOf`) . headBranch

    isActualAuthor login
        | "restyled-io" `isPrefixOf` login = False
        | "[bot]" `isSuffixOf` login = False
        | otherwise = True

simpleAuthor :: PullRequest -> Text
simpleAuthor = GH.untagName . GH.simpleUserLogin . pullRequestUser

headBranch :: PullRequest -> Text
headBranch = pullRequestCommitRef . pullRequestHead

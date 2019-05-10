{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Repo
    ( repoPath
    , repoPullPath
    , RepoWithStats(..)
    , repoWithStats

    -- * Repo Webhooks
    , IgnoredWebhookReason(..)
    , reasonToLogMessage
    , initializeFromWebhook
    , fetchReposByOwnerName
    )
where

import ClassyPrelude

import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model
import Yesod.Core (toPathPiece)

-- | Make a nicely-formatted @:owner\/:name@
--
-- Surprisingly, this can be valuable to have a shorter name available
--
repoPath :: OwnerName -> RepoName -> Text
repoPath owner name = toPathPiece owner <> "/" <> toPathPiece name

repoPullPath :: OwnerName -> RepoName -> PullRequestNum -> Text
repoPullPath owner name num = repoPath owner name <> "#" <> toPathPiece num

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

-- TODO: naive limiting for now
fetchReposByOwnerName :: MonadIO m => OwnerName -> SqlPersistT m [Entity Repo]
fetchReposByOwnerName owner =
    selectList [RepoOwner ==. owner] [Asc RepoName, LimitTo 10]

data IgnoredWebhookReason
    = InvalidJSON String
    | IgnoredAction PullRequestEventType
    | IgnoredEventType Text
    | OwnPullRequest Text

reasonToLogMessage :: IgnoredWebhookReason -> Text
reasonToLogMessage = \case
    InvalidJSON errs -> "invalid JSON: " <> pack errs
    IgnoredAction action -> "ignored action: " <> tshow action
    IgnoredEventType event -> "ignored event: " <> tshow event
    OwnPullRequest author -> "PR appears to be our own, author=" <> author

initializeFromWebhook
    :: MonadIO m
    => Payload
    -> SqlPersistT m (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook payload@Payload {..}
    | pAction `notElem` enqueueEvents = pure $ Left $ IgnoredAction pAction
    | not $ isActualAuthor pAuthor = pure $ Left $ OwnPullRequest pAuthor
    | otherwise = Right <$> findOrCreateRepo payload

findOrCreateRepo :: MonadIO m => Payload -> SqlPersistT m (Entity Repo)
findOrCreateRepo Payload {..} = do
    let
        repo = Repo
            { repoSvcs = pSVCS
            , repoOwner = pOwnerName
            , repoName = pRepoName
            , repoInstallationId = pInstallationId
            , repoIsPrivate = pRepoIsPrivate
            , repoDebugEnabled = False
            }

    upsert
        repo
        [ RepoInstallationId =. repoInstallationId repo
        , RepoIsPrivate =. repoIsPrivate repo
        ]

isActualAuthor :: Text -> Bool
isActualAuthor author
    | "restyled-io" `isPrefixOf` author = False
    | "[bot]" `isSuffixOf` author = False
    | otherwise = True

-- | Events that should enqueue a Restyler job
--
-- N.B. includes Closed because restyler will handle cleaning up any outstanding
-- Restyled PRs when a source PR closes. That's all it will do with such a case.
--
enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized, PullRequestClosed]

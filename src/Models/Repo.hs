{-# LANGUAGE LambdaCase #-}

module Models.Repo
    (
    -- * Virtual attributes
      repoPath
    , repoPullPath
    , repoIsDebug

    -- * Queries
    , fetchReposByOwnerName
    , fetchRepoForJob

    -- * Decorated
    , RepoWithStats(..)
    , repoWithStats

    -- * Repo Webhooks
    , IgnoredWebhookReason(..)
    , reasonToLogMessage
    , initializeFromWebhook
    )
where

import Restyled.Prelude

import qualified Data.Text as T
import Models.DB
import Settings

-- | Make a nicely-formatted @:owner\/:name@
--
-- Surprisingly, this can be valuable to have a shorter name available
--
repoPath :: OwnerName -> RepoName -> Text
repoPath owner name = toPathPiece owner <> "/" <> toPathPiece name

repoPullPath :: OwnerName -> RepoName -> PullRequestNum -> Text
repoPullPath owner name num = repoPath owner name <> "#" <> toPathPiece num

repoIsDebug :: AppSettings -> Repo -> Bool
repoIsDebug AppSettings {..} Repo {..} =
    appLogLevel == LevelDebug || repoDebugEnabled

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

fetchRepoForJob :: MonadIO m => Job -> SqlPersistT m (Maybe (Entity Repo))
fetchRepoForJob Job {..} = selectFirst
    [RepoSvcs ==. jobSvcs, RepoOwner ==. jobOwner, RepoName ==. jobRepo]
    []

data IgnoredWebhookReason
    = InvalidJSON String
    | IgnoredAction PullRequestEventType
    | IgnoredEventType Text
    | OwnPullRequest Text
    | RepoNotFound OwnerName RepoName

reasonToLogMessage :: IgnoredWebhookReason -> String
reasonToLogMessage = \case
    InvalidJSON errs -> "invalid JSON: " <> errs
    IgnoredAction action -> "ignored action: " <> show action
    IgnoredEventType event -> "ignored event: " <> show event
    OwnPullRequest author ->
        "PR appears to be our own, author=" <> unpack author
    RepoNotFound owner name ->
        "Repo not found: " <> unpack (repoPath owner name)

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
    | "restyled-io" `T.isPrefixOf` author = False
    | "[bot]" `T.isSuffixOf` author = False
    | otherwise = True

-- | Events that should enqueue a Restyler job
--
-- N.B. includes Closed because restyler will handle cleaning up any outstanding
-- Restyled PRs when a source PR closes. That's all it will do with such a case.
--
enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized, PullRequestClosed]

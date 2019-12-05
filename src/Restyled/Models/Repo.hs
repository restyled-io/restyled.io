{-# LANGUAGE LambdaCase #-}

module Restyled.Models.Repo
    (
    -- * Virtual attributes
      repoPath
    , repoPullPath
    , repoIsDebug
    , repoInstallationToken

    -- * Queries
    , fetchReposByOwnerName
    , fetchRepoForJob
    , fetchMarketplaceAccountForRepoT
    , fetchMarketplaceEnabledRepoIds

    -- * Decorated
    , RepoWithStats(..)
    , repoWithStats

    -- * Repo Webhooks
    , IgnoredWebhookReason(..)
    , reasonToLogMessage
    , initializeFromWebhook

    -- * Helpers useful to other modules
    , upsertRepo
    )
where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Models.DB
import Restyled.Settings

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

repoInstallationToken
    :: (HasCallStack, MonadIO m) => AppSettings -> Repo -> m AccessToken
repoInstallationToken AppSettings {..} Repo {..} = do
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    untryIO $ accessTokenFor auth repoInstallationId

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

fetchMarketplaceAccountForRepoT
    :: MonadIO m => Repo -> MaybeT (SqlPersistT m) (Entity MarketplaceAccount)
fetchMarketplaceAccountForRepoT Repo {..} =
    selectFirstT [MarketplaceAccountGithubLogin ==. nameToName repoOwner] []

fetchMarketplaceEnabledRepoIds
    :: MonadIO m
    => MarketplacePlanId
    -> MarketplaceAccountId
    -> SqlPersistT m [RepoId]
fetchMarketplaceEnabledRepoIds planId accountId =
    marketplaceEnabledRepoRepo
        . entityVal
        <$$> selectList
                 [ MarketplaceEnabledRepoMarketplacePlan ==. planId
                 , MarketplaceEnabledRepoMarketplaceAccount ==. accountId
                 ]
                 []

data IgnoredWebhookReason
    = InvalidJSON String
    | IgnoredAction PullRequestEventType
    | IgnoredEventType Text
    | RestyledBotPullRequest Text
    | RepoNotFound OwnerName RepoName

reasonToLogMessage :: IgnoredWebhookReason -> String
reasonToLogMessage = \case
    InvalidJSON errs -> "invalid JSON: " <> errs
    IgnoredAction action -> "ignored action: " <> show action
    IgnoredEventType event -> "ignored event: " <> show event
    RestyledBotPullRequest author ->
        "PR created by Restyled bot, " <> unpack author
    RepoNotFound owner name ->
        "Repo not found: " <> unpack (repoPath owner name)

initializeFromWebhook
    :: MonadIO m
    => Payload
    -> SqlPersistT m (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook payload@Payload {..}
    | pAction `notElem` enqueueEvents = pure $ Left $ IgnoredAction pAction
    | isRestyledBot pAuthor = pure $ Left $ RestyledBotPullRequest pAuthor
    | otherwise = Right <$> findOrCreateRepo payload

findOrCreateRepo :: MonadIO m => Payload -> SqlPersistT m (Entity Repo)
findOrCreateRepo Payload {..} = upsertRepo Repo
    { repoSvcs = pSVCS
    , repoOwner = pOwnerName
    , repoName = pRepoName
    , repoInstallationId = pInstallationId
    , repoIsPrivate = pRepoIsPrivate
    , repoDebugEnabled = False
    , repoEnabled = True
    }

upsertRepo :: MonadIO m => Repo -> SqlPersistT m (Entity Repo)
upsertRepo repo@Repo {..} = upsert
    repo
    [ RepoInstallationId =. repoInstallationId
    , RepoIsPrivate =. repoIsPrivate
    , RepoDebugEnabled =. repoDebugEnabled
    ]

-- | Matches on @restyled-io(-{env})[bot]@.
isRestyledBot :: Text -> Bool
isRestyledBot =
    (&&) <$> ("restyled-io" `T.isPrefixOf`) <*> ("[bot]" `T.isSuffixOf`)

-- | Events that should enqueue a Restyler job
--
-- N.B. includes Closed because restyler will handle cleaning up any outstanding
-- Restyled PRs when a source PR closes. That's all it will do with such a case.
--
enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized, PullRequestClosed]

module Restyled.Models.Repo
    (
    -- * Virtual attributes
      repoPath
    , repoPullPath
    , repoIsDebug
    , repoInstallationToken

    -- * Queries
    , fetchReposByOwnerName
    , fetchMarketplaceAccountForRepoT
    , fetchMarketplaceEnabledRepoIds

    -- * Repo Webhooks
    , IgnoredWebhookReason(..)
    , reasonToLogMessage
    , initializeFromWebhook

    -- * Helpers useful to other modules
    , upsertRepo
    ) where

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
    untryIO $ githubRequest auth $ accessTokenForR repoInstallationId

-- TODO: naive limiting for now
fetchReposByOwnerName :: MonadIO m => OwnerName -> SqlPersistT m [Entity Repo]
fetchReposByOwnerName owner =
    selectList [RepoOwner ==. owner] [Asc RepoName, LimitTo 10]

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
    | IgnoredBotPullRequest Text
    | RestyledBotPullRequest Text
    | RepoNotFound OwnerName RepoName

reasonToLogMessage :: IgnoredWebhookReason -> String
reasonToLogMessage = \case
    InvalidJSON errs -> "invalid JSON: " <> errs
    IgnoredAction action -> "ignored action: " <> show action
    IgnoredEventType event -> "ignored event: " <> show event
    IgnoredBotPullRequest author ->
        "PR created by an ignored bot, " <> unpack author
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
    | pAuthor `elem` ignoredBots = pure $ Left $ IgnoredBotPullRequest pAuthor
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
    , repoRestylerImage = Nothing
    }

upsertRepo :: MonadIO m => Repo -> SqlPersistT m (Entity Repo)
upsertRepo repo@Repo {..} = upsert
    repo
    [ RepoInstallationId =. repoInstallationId
    , RepoIsPrivate =. repoIsPrivate
    , RepoDebugEnabled =. repoDebugEnabled
    ]

-- | Bots whose PRs we choose to ignore
--
-- Ignoring all bots is too much, but some bots create PRs that are just never
-- useful to Restyle. And some of those bots create PRs that are /problematic/
-- to Restyle.
--
-- - @pull[bot]@
--
--   A bot to keep Forks up to date. It's unlikely anyone would want to keep
--   their fork up to date *and* restyle on top of it, that's just a recipe for
--   merge conflicts later. These PRs are typically also massive and so tie up
--   Restyle Machines needlessly.
--
-- __TODO__: it would be better to ignore this after accepting the Webhook, so
-- we could set a skipped-job and provide feedback to users. In order to
-- accomplish this, we'd need to send author information deeper into the system.
--
ignoredBots :: [Text]
ignoredBots = ["pull[bot]"]

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

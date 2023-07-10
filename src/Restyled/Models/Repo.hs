module Restyled.Models.Repo
  ( -- * Virtual attributes
    repoPath
  , repoPullPath

    -- * Queries
  , fetchReposByOwnerName
  , fetchMarketplaceAccountForRepoT
  , fetchMarketplaceEnabledRepoIds

    -- * Helpers useful to other modules
  , upsertRepo
  ) where

import Restyled.Prelude

import Restyled.Models.DB

-- | Make a nicely-formatted @:owner\/:name@
--
-- Surprisingly, this can be valuable to have a shorter name available
repoPath :: OwnerName -> RepoName -> Text
repoPath owner name = toPathPiece owner <> "/" <> toPathPiece name

repoPullPath :: OwnerName -> RepoName -> PullRequestNum -> Text
repoPullPath owner name num = repoPath owner name <> "#" <> toPathPiece num

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

upsertRepo :: MonadIO m => Repo -> SqlPersistT m (Entity Repo)
upsertRepo repo@Repo {..} =
  upsert
    repo
    [ RepoInstallationId =. repoInstallationId
    , RepoIsPrivate =. repoIsPrivate
    , RepoDebugEnabled =. repoDebugEnabled
    ]

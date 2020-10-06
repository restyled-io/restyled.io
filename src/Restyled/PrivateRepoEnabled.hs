module Restyled.PrivateRepoEnabled
    ( PrivateRepoEnabled(..)
    , enableMarketplaceRepo
    , disableMarketplaceRepo
    )
where

import Restyled.Prelude

import Data.List (genericLength)
import Restyled.Models
import Restyled.PrivateRepoAllowance

data PrivateRepoEnabled
    = PrivateRepoEnabled
    | PrivateRepoNotAllowed
    | PrivateRepoLimited

enableMarketplaceRepo
    :: MonadIO m => Entity Repo -> SqlPersistT m (Maybe PrivateRepoEnabled)
enableMarketplaceRepo (Entity repoId repo) = runMaybeT $ do
    account <- fetchMarketplaceAccountForRepoT repo
    enableMarketplaceRepoIdForAccountT repoId account

enableMarketplaceRepoIdForAccountT
    :: MonadIO m
    => RepoId
    -> Entity MarketplaceAccount
    -> MaybeT (SqlPersistT m) PrivateRepoEnabled
enableMarketplaceRepoIdForAccountT repoId (Entity accountId account) = do
    Entity planId plan <- getEntityT $ marketplaceAccountMarketplacePlan account

    lift
        $ enableMarketplaceRepoForPlan planId accountId repoId
        $ marketplacePlanPrivateRepoAllowance plan

enableMarketplaceRepoForPlan
    :: MonadIO m
    => MarketplacePlanId
    -> MarketplaceAccountId
    -> RepoId
    -> PrivateRepoAllowance
    -> SqlPersistT m PrivateRepoEnabled
enableMarketplaceRepoForPlan planId accountId repoId = \case
    PrivateRepoAllowanceNone -> pure PrivateRepoNotAllowed
    PrivateRepoAllowanceUnlimited -> pure PrivateRepoEnabled
    PrivateRepoAllowanceLimited limit -> do
        enabledRepoIds <-
            marketplaceEnabledRepoRepo
            . entityVal
            <$$> selectList
                     [ MarketplaceEnabledRepoMarketplacePlan ==. planId
                     , MarketplaceEnabledRepoMarketplaceAccount ==. accountId
                     ]
                     []

        result <- checkEnabledRepos enabledRepoIds limit
        pure $ if result then PrivateRepoEnabled else PrivateRepoLimited
  where
    checkEnabledRepos enabledRepoIds limit
        | repoId `elem` enabledRepoIds = pure True
        | genericLength enabledRepoIds >= limit = pure False
        | otherwise = True <$ insert MarketplaceEnabledRepo
            { marketplaceEnabledRepoMarketplacePlan = planId
            , marketplaceEnabledRepoMarketplaceAccount = accountId
            , marketplaceEnabledRepoRepo = repoId
            }

disableMarketplaceRepo :: MonadIO m => Entity Repo -> SqlPersistT m ()
disableMarketplaceRepo repo = void $ runMaybeT $ do
    account <- fetchMarketplaceAccountForRepoT $ entityVal repo
    plan <- getEntityT $ marketplaceAccountMarketplacePlan $ entityVal account
    lift $ deleteWhere
        [ MarketplaceEnabledRepoMarketplacePlan ==. entityKey plan
        , MarketplaceEnabledRepoMarketplaceAccount ==. entityKey account
        , MarketplaceEnabledRepoRepo ==. entityKey repo
        ]

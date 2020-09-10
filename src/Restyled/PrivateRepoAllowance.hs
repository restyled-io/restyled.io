module Restyled.PrivateRepoAllowance
    ( PrivateRepoAllowance(..)
    , privateRepoAllowance

    -- * Enable/disable
    , PrivateRepoEnabled(..)
    , enableMarketplaceRepo
    , enableMarketplaceRepoForUser
    , disableMarketplaceRepoForUser
    )
where

import Restyled.Prelude

import Data.List (genericLength)
import Restyled.Models

data PrivateRepoEnabled
    = PrivateRepoEnabled
    | PrivateRepoNotAllowed
    | PrivateRepoLimited

enableMarketplaceRepo
    :: MonadIO m => Entity Repo -> SqlPersistT m (Maybe PrivateRepoEnabled)
enableMarketplaceRepo (Entity repoId repo) = runMaybeT $ do
    account <- fetchMarketplaceAccountForRepoT repo
    enableMarketplaceRepoIdForAccountT repoId account

enableMarketplaceRepoForUser
    :: MonadIO m
    => Entity Repo
    -> Entity User
    -> SqlPersistT m (Maybe PrivateRepoEnabled)
enableMarketplaceRepoForUser repo@(Entity repoId _) user = runMaybeT $ do
    account <- fetchMarketplaceAccountForRepoUserT repo user
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
        $ privateRepoAllowance
        $ marketplacePlanGithubId plan

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

disableMarketplaceRepoForUser
    :: MonadIO m => Entity Repo -> Entity User -> SqlPersistT m ()
disableMarketplaceRepoForUser repo user = void $ runMaybeT $ do
    account <- fetchMarketplaceAccountForRepoUserT repo user
    plan <- getEntityT $ marketplaceAccountMarketplacePlan $ entityVal account
    lift $ deleteWhere
        [ MarketplaceEnabledRepoMarketplacePlan ==. entityKey plan
        , MarketplaceEnabledRepoMarketplaceAccount ==. entityKey account
        , MarketplaceEnabledRepoRepo ==. entityKey repo
        ]

fetchMarketplaceAccountForRepoUserT
    :: MonadIO m
    => Entity Repo
    -> Entity User
    -> MaybeT (SqlPersistT m) (Entity MarketplaceAccount)
fetchMarketplaceAccountForRepoUserT repo user = do
    repoAccount <- fetchMarketplaceAccountForRepoT $ entityVal repo
    userAccount <- fetchMarketplaceAccountForUserT $ entityVal user
    userAccount <$ guard (userAccount == repoAccount)

data PrivateRepoAllowance
    = PrivateRepoAllowanceNone
    | PrivateRepoAllowanceUnlimited
    | PrivateRepoAllowanceLimited Natural

privateRepoAllowance :: Int -> PrivateRepoAllowance
privateRepoAllowance = \case
    -- Manually-managed "Friends & Family" plan
    0 -> PrivateRepoAllowanceUnlimited
    -- Temporary "Early Adopter" plan
    2178 -> PrivateRepoAllowanceUnlimited
    -- "Unlimited" private repo plan
    2553 -> PrivateRepoAllowanceUnlimited
    -- "Unlimited" private repo plan, newly priced
    3240 -> PrivateRepoAllowanceUnlimited
    -- "Solo", single private repo plan
    2695 -> PrivateRepoAllowanceLimited 1

    -- All other plans
    _ -> PrivateRepoAllowanceNone

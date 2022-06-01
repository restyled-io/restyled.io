module Restyled.PrivateRepoEnabled
    ( PrivateRepoEnabled(..)
    , enableMarketplaceRepo
    , disableMarketplaceRepo
    ) where

import Restyled.Prelude

import Restyled.Models
import Restyled.PrivateRepoAllowance

data PrivateRepoEnabled
    = PrivateRepoEnabled
    | PrivateRepoNotAllowed
    | PrivateRepoLimited
    | PrivateRepoAccountExpired UTCTime

enableMarketplaceRepo
    :: MonadIO m => Entity Repo -> SqlPersistT m (Maybe PrivateRepoEnabled)
enableMarketplaceRepo (Entity repoId repo) = runMaybeT $ do
    now <- lift getCurrentTime
    account <- fetchMarketplaceAccountForRepoT repo

    let mExpiresAt = marketplaceAccountExpiresAt $ entityVal account
        mExpiredAt = do
            expiresAt <- mExpiresAt
            expiresAt <$ guard (now > expiresAt)

    case mExpiredAt of
        Nothing -> enableMarketplaceRepoIdForAccountT repoId account
        Just expiredAt -> pure $ PrivateRepoAccountExpired expiredAt

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

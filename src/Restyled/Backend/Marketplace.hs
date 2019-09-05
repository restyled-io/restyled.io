{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Marketplace
    (
    -- * Checking purchased features
      MarketplacePlanAllows(..)
    , marketplacePlanAllows
    , MarketplacePlanLimitation(..)
    , whenMarketplacePlanForbids

    -- * Helpers useful outside this module
    , fetchDiscountMarketplacePlan
    , isPrivateRepoPlan

    -- * Main loop
    , synchronizeMarketplacePlans

    -- * One iteration
    , runSynchronize
    )
where

import Restyled.Prelude

import Data.List (genericLength)
import Restyled.Models
import Restyled.Settings

import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH

synchronizeMarketplacePlans
    :: (HasCallStack, HasLogFunc env, HasSettings env, HasDB env) => RIO env a
synchronizeMarketplacePlans = do
    handleAny (logWarn . displayShow) runSynchronize
    liftIO $ threadDelay $ 5 * 60 * 1000000
    synchronizeMarketplacePlans

runSynchronize
    :: (HasCallStack, HasLogFunc env, HasSettings env, HasDB env) => RIO env ()
runSynchronize = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing

    logInfo "Synchronizing GitHub Marketplace data"
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    plans <- untryIO $ GH.marketplaceListingPlans auth useStubbed

    logInfo $ "Synchronizing " <> displayShow (length plans) <> " plans"
    synchronizedAccountIds <- for plans $ \plan -> do
        planId <- runDB $ synchronizePlan plan
        accounts <-
            untryIO
            $ GH.marketplaceListingPlanAccounts auth useStubbed
            $ GH.marketplacePlanId plan

        logInfo
            $ "Synchronizing "
            <> displayShow (length accounts)
            <> " accounts with plan "
            <> displayShow (GH.marketplacePlanName plan)
        traverse (runDB . synchronizeAccount planId) accounts

    runDB $ deleteUnsynchronized $ vconcat synchronizedAccountIds
    logInfo "GitHub Marketplace data synchronized"

synchronizePlan
    :: MonadIO m => GH.MarketplacePlan -> SqlPersistT m MarketplacePlanId
synchronizePlan plan = entityKey <$> upsert
    MarketplacePlan
        { marketplacePlanGithubId = untagId $ GH.marketplacePlanId plan
        , marketplacePlanName = GH.marketplacePlanName plan
        , marketplacePlanDescription = GH.marketplacePlanDescription plan
        }
    [ MarketplacePlanName =. GH.marketplacePlanName plan
    , MarketplacePlanDescription =. GH.marketplacePlanDescription plan
    ]

synchronizeAccount
    :: MonadIO m
    => MarketplacePlanId
    -> GH.MarketplaceAccount
    -> SqlPersistT m MarketplaceAccountId
synchronizeAccount planId account = entityKey <$> upsert
    MarketplaceAccount
        { marketplaceAccountGithubId = GH.marketplaceAccountId account
        , marketplaceAccountGithubLogin = GH.marketplaceAccountLogin account
        , marketplaceAccountMarketplacePlan = planId
        }
    [MarketplaceAccountMarketplacePlan =. planId]

deleteUnsynchronized
    :: HasLogFunc env => [MarketplaceAccountId] -> SqlPersistT (RIO env) ()
deleteUnsynchronized synchronizedAccountIds = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan
    unsynchronizedAccounts <- selectList
        [ MarketplaceAccountId /<-. synchronizedAccountIds
        , MarketplaceAccountMarketplacePlan !=. planId
        ]
        []

    lift
        $ logInfo
        $ "Deleting "
        <> displayShow (length unsynchronizedAccounts)
        <> " unsynchronized accounts"
    deleteWhere [MarketplaceAccountId <-. map entityKey unsynchronizedAccounts]

fetchDiscountMarketplacePlan
    :: MonadIO m => SqlPersistT m (Entity MarketplacePlan)
fetchDiscountMarketplacePlan = upsert
    plan
    [ MarketplacePlanName =. marketplacePlanName
    , MarketplacePlanDescription =. marketplacePlanDescription
    ]
  where
    plan@MarketplacePlan {..} = MarketplacePlan
        { marketplacePlanGithubId = 0
        , marketplacePlanName = "Friends & Family"
        , marketplacePlanDescription = "Manually managed discount plan"
        }

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation
    deriving (Eq, Show)

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly
    | MarketplacePlanMaxRepos
    deriving (Eq, Show)

marketplacePlanAllows
    :: MonadIO m => Entity Repo -> SqlPersistT m MarketplacePlanAllows
marketplacePlanAllows repo@(Entity _ Repo {..})
    | repoIsPrivate = marketplacePlanAllowsPrivate repo
    | otherwise = pure MarketplacePlanAllows

marketplacePlanAllowsPrivate
    :: MonadIO m => Entity Repo -> SqlPersistT m MarketplacePlanAllows
marketplacePlanAllowsPrivate (Entity repoId Repo {..}) =
    fromMaybeM planNotFound $ runMaybeT $ do
        Entity accountId MarketplaceAccount {..} <- selectFirstT
            [MarketplaceAccountGithubLogin ==. nameToName repoOwner]
            []
        Entity planId MarketplacePlan {..} <- getEntityT
            marketplaceAccountMarketplacePlan

        lift $ case privateRepoAllowance marketplacePlanGithubId of
            PrivateRepoAllowanceNone ->
                pure $ MarketplacePlanForbids MarketplacePlanPublicOnly
            PrivateRepoAllowanceUnlimited -> pure MarketplacePlanAllows
            PrivateRepoAllowanceLimited limit -> do
                result <- enableMarketplaceRepo planId accountId repoId limit
                pure $ if result
                    then MarketplacePlanAllows
                    else MarketplacePlanForbids MarketplacePlanMaxRepos
  where
    planNotFound :: Applicative f => f MarketplacePlanAllows
    planNotFound = pure $ MarketplacePlanForbids MarketplacePlanNotFound

enableMarketplaceRepo
    :: MonadIO m
    => MarketplacePlanId
    -> MarketplaceAccountId
    -> RepoId
    -> Natural
    -> SqlPersistT m Bool
enableMarketplaceRepo planId accountId repoId limit = do
    repos <- selectList
        [ MarketplaceEnabledRepoMarketplacePlan ==. planId
        , MarketplaceEnabledRepoMarketplaceAccount ==. accountId
        ]
        []
    checkEnabledRepos $ map (marketplaceEnabledRepoRepo . entityVal) repos
  where
    checkEnabledRepos enabledRepoIds
        | repoId `elem` enabledRepoIds = pure True
        | genericLength enabledRepoIds >= limit = pure False
        | otherwise = True <$ insert MarketplaceEnabledRepo
            { marketplaceEnabledRepoMarketplacePlan = planId
            , marketplaceEnabledRepoMarketplaceAccount = accountId
            , marketplaceEnabledRepoRepo = repoId
            }

whenMarketplacePlanForbids
    :: Applicative f
    => MarketplacePlanAllows
    -> (MarketplacePlanLimitation -> f ())
    -> f ()
whenMarketplacePlanForbids MarketplacePlanAllows _ = pure ()
whenMarketplacePlanForbids (MarketplacePlanForbids limitation) f = f limitation

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
    -- "Solo", single private repo plan
    2695 -> PrivateRepoAllowanceLimited 1

    -- All other plans
    _ -> PrivateRepoAllowanceNone

isPrivateRepoPlan :: MarketplacePlan -> Bool
isPrivateRepoPlan MarketplacePlan {..} =
    case privateRepoAllowance marketplacePlanGithubId of
        PrivateRepoAllowanceNone -> False
        PrivateRepoAllowanceUnlimited -> True
        PrivateRepoAllowanceLimited _ -> True

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

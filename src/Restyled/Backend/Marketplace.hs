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
    , runSynchronize
    , runSynchronizeOnce
    )
where

import Restyled.Prelude

import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Settings

import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH

runSynchronize
    :: (HasCallStack, HasLogFunc env, HasSettings env, HasDB env) => RIO env a
runSynchronize = do
    handleAny (logWarn . displayShow) runSynchronizeOnce
    liftIO $ threadDelay $ 5 * 60 * 1000000
    runSynchronize

runSynchronizeOnce
    :: (HasCallStack, HasLogFunc env, HasSettings env, HasDB env) => RIO env ()
runSynchronizeOnce = do
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

    let accountIds = map entityKey unsynchronizedAccounts
    deleteWhere [MarketplaceEnabledRepoMarketplaceAccount <-. accountIds]
    deleteWhere [MarketplaceAccountId <-. accountIds]

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
marketplacePlanAllowsPrivate repo = do
    mEnabled <- enableMarketplaceRepo repo

    pure $ case mEnabled of
        Nothing -> MarketplacePlanForbids MarketplacePlanNotFound
        Just PrivateRepoEnabled -> MarketplacePlanAllows
        Just PrivateRepoNotAllowed ->
            MarketplacePlanForbids MarketplacePlanPublicOnly
        Just PrivateRepoLimited ->
            MarketplacePlanForbids MarketplacePlanMaxRepos

whenMarketplacePlanForbids
    :: Applicative f
    => MarketplacePlanAllows
    -> (MarketplacePlanLimitation -> f ())
    -> f ()
whenMarketplacePlanForbids MarketplacePlanAllows _ = pure ()
whenMarketplacePlanForbids (MarketplacePlanForbids limitation) f = f limitation

isPrivateRepoPlan :: MarketplacePlan -> Bool
isPrivateRepoPlan MarketplacePlan {..} =
    case privateRepoAllowance marketplacePlanGithubId of
        PrivateRepoAllowanceNone -> False
        PrivateRepoAllowanceUnlimited -> True
        PrivateRepoAllowanceLimited _ -> True

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

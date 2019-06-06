module Backend.Marketplace
    (
    -- * Checking purchased features
      MarketplacePlanAllows(..)
    , marketplacePlanAllows
    , MarketplacePlanLimitation(..)
    , whenMarketplacePlanForbids
    , isPrivateRepoPlan

    -- * Main loop
    , synchronizeMarketplacePlans

    -- * One iteration
    , runSynchronize
    )
where

import Backend.Import

import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH

synchronizeMarketplacePlans
    :: (HasLogFunc env, HasSettings env, HasDB env) => RIO env a
synchronizeMarketplacePlans = do
    handleAny (logWarn . displayShow) runSynchronize
    liftIO $ threadDelay $ 5 * 60 * 1000000
    synchronizeMarketplacePlans

runSynchronize :: (HasLogFunc env, HasSettings env, HasDB env) => RIO env ()
runSynchronize = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing

    logInfo "Synchronizing GitHub Marketplace data"
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    plans <- untryIO $ GH.marketplaceListingPlans auth useStubbed

    synchronizedAccountIds <- for plans $ \plan -> do
        planId <- runDB $ synchronizePlan plan

        accounts <-
            untryIO
            $ GH.marketplaceListingPlanAccounts auth useStubbed
            $ GH.marketplacePlanId plan

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

    lift $ logInfo $ "Deleting unsynchronized accounts: " <> displayShow
        (map (marketplaceAccountGithubLogin . entityVal) unsynchronizedAccounts)

    deleteWhere [MarketplaceAccountId <-. map entityKey unsynchronizedAccounts]

fetchDiscountMarketplacePlan
    :: MonadIO m => SqlPersistT m (Entity MarketplacePlan)
fetchDiscountMarketplacePlan =
    fromJustNoteM "Discount Plan must exist"
        =<< selectFirst [MarketplacePlanGithubId ==. 0] []

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly

-- | Current, naive @'MarketplacePlan'@ limitations
marketplacePlanAllows
    :: MonadIO m => Entity Repo -> SqlPersistT m MarketplacePlanAllows
marketplacePlanAllows (Entity _ Repo {..}) = do
    mPlan <- fetchMarketplacePlanByLogin $ nameToName repoOwner

    pure $ case (repoIsPrivate, mPlan) of
        (False, _) -> MarketplacePlanAllows
        (True, Nothing) -> MarketplacePlanForbids MarketplacePlanNotFound
        (True, Just plan)
            | isPrivateRepoPlan plan -> MarketplacePlanAllows
            | otherwise -> MarketplacePlanForbids MarketplacePlanPublicOnly

whenMarketplacePlanForbids
    :: Applicative f
    => MarketplacePlanAllows
    -> (MarketplacePlanLimitation -> f ())
    -> f ()
whenMarketplacePlanForbids MarketplacePlanAllows _ = pure ()
whenMarketplacePlanForbids (MarketplacePlanForbids limitation) f = f limitation

isPrivateRepoPlan :: MarketplacePlan -> Bool
isPrivateRepoPlan MarketplacePlan {..} =
    marketplacePlanGithubId `elem` privateRepoPlanGitHubIds

privateRepoPlanGitHubIds :: [Int]
privateRepoPlanGitHubIds =
    [ 0 -- Manually-managed "Friends & Family" plan
    , 2178 -- Temporary "Early Adopter" plan
    ]

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

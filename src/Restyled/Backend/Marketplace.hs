module Restyled.Backend.Marketplace
    (
    -- * Checking purchased features
      MarketplacePlanAllows(..)
    , marketplacePlanAllows
    , MarketplacePlanLimitation(..)
    , whenMarketplacePlanForbids

    -- * Helpers useful outside this module
    , isPrivateRepoPlan

    -- * Sync
    , runSynchronize
    )
where

import Restyled.Prelude

import Restyled.Backend.DiscountMarketplacePlan
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.PrivateRepoEnabled
import Restyled.Settings

import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH

runSynchronize
    :: (HasCallStack, HasLogFunc env, HasSettings env, HasDB env) => RIO env ()
runSynchronize = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing

    logInfo "Synchronizing GitHub Marketplace data"
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    plans <- untryIO $ GH.marketplaceListingPlans auth useStubbed

    logDebug $ "Synchronizing " <> displayShow (length plans) <> " plans"
    synchronizedAccountIds <- for plans $ \plan -> do
        planId <- runDB $ synchronizePlan plan
        synchronizePlanAccounts plan planId
            `catchAny` synchronizePlanAccountsError plan planId

    runDB $ deleteUnsynchronized $ vconcat synchronizedAccountIds
    logInfo "GitHub Marketplace data synchronized"

synchronizePlanAccounts
    :: (HasLogFunc env, HasSettings env, HasDB env)
    => GH.MarketplacePlan
    -> MarketplacePlanId
    -> RIO env (Vector MarketplaceAccountId)
synchronizePlanAccounts plan planId = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey

    accounts <-
        untryIO
        $ GH.marketplaceListingPlanAccounts auth useStubbed
        $ GH.marketplacePlanId plan

    logDebug
        $ "Synchronizing "
        <> displayShow (length accounts)
        <> " Accounts for "
        <> displayMarketplacePlan plan

    logPlanChange plan (length accounts)
        =<< runDB (count [MarketplaceAccountMarketplacePlan ==. planId])

    traverse (runDB . synchronizeAccount planId) accounts

synchronizePlanAccountsError
    :: (HasLogFunc env, HasDB env)
    => GH.MarketplacePlan
    -> MarketplacePlanId
    -> SomeException
    -> RIO env (Vector MarketplaceAccountId)
synchronizePlanAccountsError plan planId ex = do
    logError $ displayShow ex
    logError
        $ "Error synchronizing Accounts for "
        <> displayMarketplacePlan plan
        <> ", maintaining all current Accounts"

    runDB
        $ V.fromList
        <$> selectKeysList [MarketplaceAccountMarketplacePlan ==. planId] []

displayMarketplacePlan :: GH.MarketplacePlan -> Utf8Builder
displayMarketplacePlan plan =
    "Plan "
        <> display (toPathPart $ GH.marketplacePlanId plan)
        <> " ("
        <> display (GH.marketplacePlanName plan)
        <> ", "
        <> display (GH.marketplacePlanState plan)
        <> ")"

logPlanChange
    :: HasLogFunc env
    => GH.MarketplacePlan
    -> Int -- ^ New count
    -> Int -- ^ Old count
    -> RIO env ()
logPlanChange plan newCount oldCount = case newCount `compare` oldCount of
    LT -> logWarn $ prefix <> "lost" <> suffix
    EQ -> logInfo $ prefix <> "no accounts lost or gained"
    GT -> logInfo $ prefix <> "gained" <> suffix
  where
    prefix = displayMarketplacePlan plan <> ": "
    suffix = display $ " " <> pluralize "account" "accounts" diff
    diff = abs $ newCount - oldCount

synchronizePlan
    :: MonadIO m => GH.MarketplacePlan -> SqlPersistT m MarketplacePlanId
synchronizePlan plan = entityKey <$> upsert
    -- Initialize unknown plans as non-private, we'll manually change that after
    -- it gets created on the first sync
    MarketplacePlan
        { marketplacePlanGithubId = Just $ untagId $ GH.marketplacePlanId plan
        , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceNone
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
        , marketplaceAccountGithubType = GH.marketplaceAccountType account
        , marketplaceAccountEmail = GH.marketplaceAccountEmail account
        , marketplaceAccountBillingEmail =
            GH.marketplaceAccountOrganizationBillingEmail account
        , marketplaceAccountMarketplacePlan = planId
        }
    [ MarketplaceAccountGithubType =. GH.marketplaceAccountType account
    , MarketplaceAccountEmail =. GH.marketplaceAccountEmail account
    , MarketplaceAccountBillingEmail
        =. GH.marketplaceAccountOrganizationBillingEmail account
    , MarketplaceAccountMarketplacePlan =. planId
    ]

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
        $ logDebug
        $ "Deleting "
        <> displayShow (length unsynchronizedAccounts)
        <> " unsynchronized accounts"

    let accountIds = map entityKey unsynchronizedAccounts
    deleteWhere [MarketplaceEnabledRepoMarketplaceAccount <-. accountIds]
    deleteWhere [MarketplaceAccountId <-. accountIds]

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation
    deriving stock (Eq, Show)

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly
    | MarketplacePlanMaxRepos
    deriving stock (Eq, Show)

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
    case marketplacePlanPrivateRepoAllowance of
        PrivateRepoAllowanceNone -> False
        PrivateRepoAllowanceUnlimited -> True
        PrivateRepoAllowanceLimited _ -> True

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

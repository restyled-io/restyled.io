module Restyled.SyncMarketplace
    ( App
    , loadApp
    , syncMarketplace
    ) where

import Restyled.Prelude

import Control.Retry (exponentialBackoff, limitRetries, retrying)
import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH
import Restyled.Env
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.UsCents

data AppSettings = AppSettings
    { appDatabaseConf :: PostgresConf
    , appStatementTimeout :: Maybe Integer
    , appLogLevel :: LogLevel
    , appGitHubAppId :: GitHubAppId
    , appGitHubAppKey :: GitHubAppKey
    , appStubMarketplaceListing :: Bool
    }

class HasSettings env where
    settingsL :: Lens' env AppSettings

instance HasSettings AppSettings where
    settingsL = id

loadSettings :: IO AppSettings
loadSettings =
    parse
        $ AppSettings
        <$> (PostgresConf
            <$> var nonempty "DATABASE_URL" (def defaultDatabaseURL)
            <*> var auto "PGPOOLSTRIPES" (def 1)
            <*> var auto "PGPOOLIDLETIMEOUT" (def 20)
            <*> var auto "PGPOOLSIZE" (def 10)
            )
        <*> optional (var auto "STATEMENT_TIMEOUT" mempty)
        <*> var logLevel "LOG_LEVEL" (def LevelInfo)
        <*> var githubId "GITHUB_APP_ID" mempty
        <*> var nonempty "GITHUB_APP_KEY" mempty
        <*> switch "STUB_MARKETPLACE_LISTING" mempty

defaultDatabaseURL :: ByteString
defaultDatabaseURL = "postgres://postgres:password@localhost:5432/restyled"

data App = App
    { appLogFunc :: LogFunc
    , appSettings :: AppSettings
    , appSqlPool :: ConnectionPool
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasSettings App where
    settingsL = lens appSettings $ \x y -> x { appSettings = y }

instance HasSqlPool App where
    sqlPoolL = lens appSqlPool $ \x y -> x { appSqlPool = y }

loadApp :: IO App
loadApp = do
    appSettings@AppSettings {..} <- loadSettings
    appLogFunc <- terminalLogFunc stdout appLogLevel
    appSqlPool <- runRIO appLogFunc
        $ createConnectionPool appDatabaseConf appStatementTimeout
    pure App { .. }

syncMarketplace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSettings env
       , HasSqlPool env
       )
    => m ()
syncMarketplace = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing

    logInfo "Synchronizing GitHub Marketplace data"
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey
    plans <- retryWithBackoff $ GH.marketplaceListingPlans auth useStubbed

    logDebug $ "Synchronizing " <> displayShow (length plans) <> " plans"
    synchronizedAccountIds <- for plans $ \plan -> do
        planId <- runDB $ synchronizePlan plan
        synchronizePlanAccounts plan planId
            `catchAny` synchronizePlanAccountsError plan planId

    runDB $ deleteUnsynchronized $ vconcat synchronizedAccountIds
    logInfo "GitHub Marketplace data synchronized"

synchronizePlanAccounts
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSettings env
       , HasSqlPool env
       )
    => GH.MarketplacePlan
    -> MarketplacePlanId
    -> m (Vector MarketplaceAccountId)
synchronizePlanAccounts plan planId = do
    AppSettings {..} <- view settingsL
    let useStubbed = appStubMarketplaceListing
    auth <- liftIO $ authJWTMax appGitHubAppId appGitHubAppKey

    accounts <-
        retryWithBackoff
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
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasSqlPool env)
    => GH.MarketplacePlan
    -> MarketplacePlanId
    -> SomeException
    -> m (Vector MarketplaceAccountId)
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
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => GH.MarketplacePlan
    -> Int -- ^ New count
    -> Int -- ^ Old count
    -> m ()
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
        , marketplacePlanMonthlyRevenue = fromCents
            $ GH.marketplacePlanMonthlyPriceInCents plan
        , marketplacePlanRetired = GH.marketplacePlanState plan == "retired"
        }
    [ MarketplacePlanName =. GH.marketplacePlanName plan
    , MarketplacePlanDescription =. GH.marketplacePlanDescription plan
    , MarketplacePlanMonthlyRevenue
        =. fromCents (GH.marketplacePlanMonthlyPriceInCents plan)
    , MarketplacePlanRetired =. GH.marketplacePlanState plan == "retired"
    ]

synchronizeAccount
    :: MonadIO m
    => MarketplacePlanId
    -> GH.MarketplaceAccount
    -> SqlPersistT m MarketplaceAccountId
synchronizeAccount planId account = entityKey <$> upsert
    MarketplaceAccount
        { marketplaceAccountGithubId = Just $ GH.marketplaceAccountId account
        , marketplaceAccountGithubLogin = GH.marketplaceAccountLogin account
        , marketplaceAccountGithubType = GH.marketplaceAccountType account
        , marketplaceAccountEmail = GH.marketplaceAccountEmail account
        , marketplaceAccountBillingEmail =
            GH.marketplaceAccountOrganizationBillingEmail account
        , marketplaceAccountMarketplacePlan = planId
        , marketplaceAccountTrialEndsAt = Nothing
        , marketplaceAccountExpiresAt = Nothing
        }
    [ MarketplaceAccountGithubType =. GH.marketplaceAccountType account
    , MarketplaceAccountEmail =. GH.marketplaceAccountEmail account
    , MarketplaceAccountBillingEmail
        =. GH.marketplaceAccountOrganizationBillingEmail account
    , MarketplaceAccountMarketplacePlan =. planId
    , MarketplaceAccountTrialEndsAt =. GH.marketplacePurchaseFreeTrialEndsOn
        (GH.marketplaceAccountMarketplacePurchase account)
    ]

deleteUnsynchronized
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => [MarketplaceAccountId]
    -> SqlPersistT m ()
deleteUnsynchronized synchronizedAccountIds = do
    nonGitHubPlanIds <- selectKeysList [MarketplacePlanGithubId ==. Nothing] []
    unsynchronizedAccounts <- selectList
        [ MarketplaceAccountId /<-. synchronizedAccountIds
        , MarketplaceAccountMarketplacePlan /<-. nonGitHubPlanIds
        ]
        []

    unless (null unsynchronizedAccounts)
        $ lift
        $ logWarn
        $ "Deleting "
        <> displayShow (length unsynchronizedAccounts)
        <> " unsynchronized accounts"

    let accountIds = map entityKey unsynchronizedAccounts
    deleteWhere [MarketplaceEnabledRepoMarketplaceAccount <-. accountIds]
    deleteWhere [MarketplaceAccountId <-. accountIds]

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

retryWithBackoff
    :: (MonadIO m, MonadReader env m, HasLogFunc env, Exception e)
    => IO (Either e a)
    -> m a
retryWithBackoff f = do
    result <- retrying
        (exponentialBackoff 1000000 <> limitRetries 5)
        (\_ -> either (\ex -> True <$ logRetry ex) (const $ pure False))
        (\_ -> liftIO f)
    either throwIO pure result
  where
    logRetry
        :: (MonadIO m, MonadReader env m, HasLogFunc env, Exception e)
        => e
        -> m ()
    logRetry ex =
        logWarn $ "Retrying (" <> fromString (displayException ex) <> ")"

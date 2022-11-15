module Restyled.SyncMarketplace
    ( App
    , runApp
    , syncMarketplace
    ) where

import Restyled.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Retry (exponentialBackoff, limitRetries, retrying)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GitHub.Endpoints.MarketplaceListing.Plans as GH
import qualified GitHub.Endpoints.MarketplaceListing.Plans.Accounts as GH
import Restyled.DB
import Restyled.Env
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.UsCents

data AppSettings = AppSettings
    { appDatabaseConf :: PostgresConf
    , appStatementTimeout :: Maybe Integer
    , appLogSettings :: LogSettings
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
        <$> postgresConf
        <*> optional (var auto "STATEMENT_TIMEOUT" mempty)
        <*> LoggingEnv.parser
        <*> var githubId "GITHUB_APP_ID" mempty
        <*> var nonempty "GITHUB_APP_KEY" mempty
        <*> switch "STUB_MARKETPLACE_LISTING" mempty

data App = App
    { appSettings :: AppSettings
    , appSqlPool :: ConnectionPool
    }

instance HasSettings App where
    settingsL = lens appSettings $ \x y -> x { appSettings = y }

instance HasSqlPool App where
    sqlPoolL = lens appSqlPool $ \x y -> x { appSqlPool = y }

runApp :: ReaderT App (LoggingT IO) a -> IO a
runApp f = do
    appSettings@AppSettings {..} <- loadSettings

    logger <- newLogger appLogSettings

    let app :: Text
        app = "sync-marketplace"

        runLogging :: LoggingT IO a -> IO a
        runLogging =
            runLoggerLoggingT logger . withThreadContext ["app" .= app]

    appSqlPool <- runLogging
        $ createConnectionPool appDatabaseConf appStatementTimeout

    runLogging $ runReaderT f App { .. }

syncMarketplace
    :: ( MonadUnliftIO m
       , MonadReader env m
       , MonadLogger m
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

    logDebug $ "Synchronizing plans" :# ["count" .= length plans]
    synchronizedAccountIds <- for plans $ \plan -> do
        planId <- runDB $ synchronizePlan plan
        synchronizePlanAccounts plan planId
            `catchAny` synchronizePlanAccountsError plan planId

    runDB $ deleteUnsynchronized $ vconcat synchronizedAccountIds
    logInfo "GitHub Marketplace data synchronized"

synchronizePlanAccounts
    :: ( MonadUnliftIO m
       , MonadReader env m
       , MonadLogger m
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
        $ "Synchronizing accounts"
        :# ["count" .= length accounts, "plan" .= marketplacePlanDetails plan]
    logPlanChange plan (length accounts)
        =<< runDB (count [MarketplaceAccountMarketplacePlan ==. planId])

    traverse (runDB . synchronizeAccount planId) accounts

synchronizePlanAccountsError
    :: (MonadUnliftIO m, MonadReader env m, MonadLogger m, HasSqlPool env)
    => GH.MarketplacePlan
    -> MarketplacePlanId
    -> SomeException
    -> m (Vector MarketplaceAccountId)
synchronizePlanAccountsError plan planId ex = do
    logError
        $ "Error synchronizing Accounts, maintaining all current Accounts"
        :# [ "exception" .= displayException ex
           , "plan" .= marketplacePlanDetails plan
           ]

    runDB
        $ V.fromList
        <$> selectKeysList [MarketplaceAccountMarketplacePlan ==. planId] []

marketplacePlanDetails :: GH.MarketplacePlan -> Value
marketplacePlanDetails plan = object
    [ "id" .= toPathPart (GH.marketplacePlanId plan)
    , "name" .= GH.marketplacePlanName plan
    , "state" .= GH.marketplacePlanState plan
    ]

logPlanChange
    :: MonadLogger m
    => GH.MarketplacePlan
    -> Int -- ^ New count
    -> Int -- ^ Old count
    -> m ()
logPlanChange plan newCount oldCount = case newCount `compare` oldCount of
    LT -> logWarn $ "Accounts lost" :# series
    EQ -> logInfo $ "No accounts lost or gained" :# series
    GT -> logInfo $ "Accounts gained" :# series
  where
    series = ["plan" .= marketplacePlanDetails plan, "diff" .= diff]
    diff = abs $ newCount - oldCount

synchronizePlan
    :: MonadIO m => GH.MarketplacePlan -> SqlPersistT m MarketplacePlanId
synchronizePlan plan = entityKey <$> upsert
    MarketplacePlan
        { marketplacePlanGithubId = Just $ untagId $ GH.marketplacePlanId plan
        , marketplacePlanPrivateRepoAllowance = inferPrivateRepoAllowanceByName
            $ GH.marketplacePlanName plan
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

inferPrivateRepoAllowanceByName :: Text -> PrivateRepoAllowance
inferPrivateRepoAllowanceByName = \case
    "Unlimited" -> PrivateRepoAllowanceNone
    "Solo" -> PrivateRepoAllowanceLimited 1
    _ -> PrivateRepoAllowanceNone

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
    :: (MonadIO m, MonadLogger m) => [MarketplaceAccountId] -> SqlPersistT m ()
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
        $ "Deleting unsynchronized accounts"
        :# ["count" .= length unsynchronizedAccounts]

    let accountIds = map entityKey unsynchronizedAccounts
    deleteWhere [MarketplaceEnabledRepoMarketplaceAccount <-. accountIds]
    deleteWhere [MarketplaceAccountId <-. accountIds]

vconcat :: Vector (Vector a) -> [a]
vconcat = V.toList . V.concat . V.toList

retryWithBackoff
    :: (MonadIO m, MonadLogger m, Exception e) => IO (Either e a) -> m a
retryWithBackoff f = do
    result <- retrying
        (exponentialBackoff 1000000 <> limitRetries 5)
        (\_ -> either (\ex -> True <$ logRetry ex) (const $ pure False))
        (\_ -> liftIO f)
    either throwIO pure result
  where
    logRetry :: (MonadLogger m, Exception e) => e -> m ()
    logRetry ex = logWarn $ "Retrying" :# ["exception" .= displayException ex]

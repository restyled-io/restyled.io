module Restyled.Development.Seeds
  ( App
  , runApp
  , seedDB
  ) where

import Restyled.Prelude hiding (First)

import Amazonka.CloudWatchLogs.CreateLogStream
import Amazonka.CloudWatchLogs.DeleteLogStream
import Amazonka.CloudWatchLogs.Types
import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Restyled.AWS (HasAWS (..))
import qualified Restyled.AWS as AWS
import Restyled.DB
import Restyled.Development.LogEvent
import Restyled.Env
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.UsCents
import System.FilePath ((<.>), (</>))
import UnliftIO.Exception.Lens (handling_)

data AppSettings = AppSettings
  { appDatabaseConf :: PostgresConf
  , appStatementTimeout :: Maybe Integer
  , appLogSettings :: LogSettings
  , appRestylerLogGroup :: Text
  , appRestylerLogStreamPrefix :: Text
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
    <*> var nonempty "RESTYLER_LOG_GROUP" (def "restyled/dev/restyler")
    <*> var nonempty "RESTYLER_LOG_STREAM_PREFIX" (def "jobs/")

data App = App
  { appSettings :: AppSettings
  , appSqlPool :: ConnectionPool
  , appAWSEnv :: AWS.Env
  }

instance HasSettings App where
  settingsL = lens appSettings $ \x y -> x {appSettings = y}

instance HasSqlPool App where
  sqlPoolL = lens appSqlPool $ \x y -> x {appSqlPool = y}

instance HasAWS App where
  awsEnvL = lens appAWSEnv $ \x y -> x {appAWSEnv = y}

runApp :: ReaderT App (ResourceT (LoggingT IO)) a -> IO a
runApp f = do
  appSettings@AppSettings {..} <- liftIO loadSettings

  logger <- newLogger appLogSettings

  let
    app :: Text
    app = "seed-db"

    runLogging :: LoggingT IO a -> IO a
    runLogging =
      runLoggerLoggingT logger . withThreadContext ["app" .= app]

  (appSqlPool, appAWSEnv) <-
    runLogging
      $ (,)
      <$> createConnectionPool appDatabaseConf appStatementTimeout
      <*> AWS.discover

  runLogging $ runResourceT $ runReaderT f App {..}

seedDB
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasSettings env
     , HasAWS env
     )
  => SqlPersistT m ()
seedDB = do
  now <- liftIO getCurrentTime

  Entity _ demo <- upsertRepo $ restyledRepo "demo"
  Entity _ restyled <- upsertRepo $ restyledRepo "restyled.io"
  Entity _ restyler <- upsertRepo $ restyledRepo "restyler"
  Entity _ _ops <- upsertRepo $ restyledRepoPrivate "ops"

  let
    readSeedOutput :: MonadIO m => String -> m [Text]
    readSeedOutput name = do
      let path = "seeds" </> "output" </> name <.> "txt"
      lines . decodeUtf8 <$> readFileBS path

  invalidArgumentOutput <- readSeedOutput "invalid-arguments"
  noDifferencesOutput <- readSeedOutput "no-differences"
  restylingOutput <- readSeedOutput "restyling"
  restylingOutputGHA <- readSeedOutput "gha1"
  restylingOutputGHAPatch <- readSeedOutput "gha-patch"
  configErrorOutput1 <- readSeedOutput "config-error1"

  seedJob demo 1 now (Just 127) invalidArgumentOutput
  seedJob restyled 1 now (Just 0) noDifferencesOutput
  seedJob restyled 2 now (Just 0) restylingOutputGHA
  seedJob restyled 3 now (Just 0) restylingOutputGHAPatch
  seedJob restyler 1 now Nothing restylingOutput
  seedJob restyler 2 now (Just 10) configErrorOutput1

  Entity discountPlanId _ <-
    findOrCreateMarketplacePlan
      MarketplacePlan
        { marketplacePlanGithubId = Nothing
        , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceUnlimited
        , marketplacePlanName = "Friends & Family"
        , marketplacePlanDescription = "Manually managed discount plan"
        , marketplacePlanMonthlyRevenue = fromCents 0
        , marketplacePlanRetired = False
        , marketplacePlanCpuShares = Nothing
        , marketplacePlanMemory = Nothing
        }

  void
    $ upsert
      Offer
        { offerName = "Friends & Family"
        , offerDetails = "Exclusive offer for Friends of the Po- Restyled."
        , offerPurchaseUrl = "https://example.com"
        , offerMarketplacePlan = discountPlanId
        }
      [OfferMarketplacePlan =. discountPlanId]

  void
    $ upsert
      MarketplaceAccount
        { marketplaceAccountGithubId = Just 50812
        , marketplaceAccountGithubLogin = "pbrisbin"
        , marketplaceAccountMarketplacePlan = discountPlanId
        , marketplaceAccountGithubType = "User"
        , marketplaceAccountEmail = Nothing
        , marketplaceAccountBillingEmail = Nothing
        , marketplaceAccountTrialEndsAt = Nothing
        , marketplaceAccountExpiresAt = Nothing
        }
      [MarketplaceAccountMarketplacePlan =. discountPlanId]

restyledRepo :: RepoName -> Repo
restyledRepo name =
  Repo
    { repoSvcs = GitHubSVCS
    , repoOwner = "restyled-io"
    , repoName = name
    , repoInstallationId = 58920
    , repoIsPrivate = False
    , repoDebugEnabled = True
    , repoEnabled = True
    , repoRestylerImage = Nothing
    }

restyledRepoPrivate :: RepoName -> Repo
restyledRepoPrivate name = (restyledRepo name) {repoIsPrivate = True}

seedJob
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasSettings env
     , HasAWS env
     )
  => Repo
  -> PullRequestNum
  -> UTCTime
  -> Maybe Int
  -> [Text]
  -> SqlPersistT m ()
seedJob Repo {..} pullRequest createdAt mExitCode untimestamped = do
  -- Start the Job
  jobId <-
    insert
      $ Job
        { jobSvcs = repoSvcs
        , jobOwner = repoOwner
        , jobRepo = repoName
        , jobPullRequest = pullRequest
        , jobCreatedAt = createdAt
        , jobUpdatedAt = createdAt
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        }

  -- Capture it "running"
  let jobLogLines = zipWith jobLogLine (msecondsFrom createdAt) untimestamped

  lift $ seedJobLogLines jobId jobLogLines

  -- Complete it if appropriate
  for_ mExitCode $ \ec -> do
    let
      mLastLogLineAt = jobLogLineCreatedAt . last <$> nonEmpty jobLogLines
      completedAt = addUTCTime 1 $ fromMaybe createdAt mLastLogLineAt

    update
      jobId
      [ JobUpdatedAt =. completedAt
      , JobCompletedAt =. Just completedAt
      , JobExitCode =. Just ec
      ]

msecondsFrom :: UTCTime -> [UTCTime]
msecondsFrom = iterate $ addUTCTime 0.001

seedJobLogLines
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasSettings env
     , HasAWS env
     )
  => JobId
  -> [JobLogLine]
  -> m ()
seedJobLogLines jobId jobLogLines = do
  AppSettings {..} <- view settingsL

  let
    logGroup = appRestylerLogGroup
    logStream = appRestylerLogStreamPrefix <> toPathPiece jobId
    events = map toInputLogEvent jobLogLines

  handling_ _ResourceNotFoundException (pure ()) $ do
    void $ AWS.send $ newDeleteLogStream logGroup logStream
    void $ AWS.send $ newCreateLogStream logGroup logStream
    result <- runExceptT $ putLogEvents logGroup logStream events
    case result of
      Left exs -> do
        logError $ pack (displayException exs) :# []
        exitFailure
      Right () -> pure ()

toInputLogEvent :: JobLogLine -> InputLogEvent
toInputLogEvent ln =
  newInputLogEvent createdMs $ ensureNonEmpty $ jobLogLineContent ln
 where
  createdMs = utcTimeToPOSIXMilliseconds $ jobLogLineCreatedAt ln
  ensureNonEmpty t
    | T.null t = " "
    | otherwise = t

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

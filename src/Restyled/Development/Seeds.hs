module Restyled.Development.Seeds
    ( App
    , runApp
    , seedDB
    ) where

import Restyled.Prelude hiding (First)

import Amazonka.CloudWatchLogs.CreateLogStream
import Amazonka.CloudWatchLogs.DeleteLogStream
import Amazonka.CloudWatchLogs.PutLogEvents
import Amazonka.CloudWatchLogs.Types
import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Restyled.AWS (HasAWS(..))
import qualified Restyled.AWS as AWS
import Restyled.DB
import Restyled.Env
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Tracing
import Restyled.UsCents

-- Fort the catching_, ignoring_ overrides
import Data.Monoid (First)
import Lens.Micro (Getting)
import Lens.Micro.Mtl (preview)
import UnliftIO.Exception (catchJust)

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
    settingsL = lens appSettings $ \x y -> x { appSettings = y }

instance HasTracingApp App where
    tracingAppL = lens (const TracingDisabled) const

instance HasTransactionId App where
    transactionIdL = lens (const Nothing) const

instance HasSqlPool App where
    sqlPoolL = lens appSqlPool $ \x y -> x { appSqlPool = y }

instance HasAWS App where
    awsEnvL = lens appAWSEnv $ \x y -> x { appAWSEnv = y }

runApp :: ReaderT App (ResourceT (LoggingT IO)) a -> IO a
runApp f = do
    appSettings@AppSettings {..} <- liftIO loadSettings

    logger <- newLogger appLogSettings

    let app :: Text
        app = "seed-db"

        runLogging :: LoggingT IO a -> IO a
        runLogging =
            runLoggerLoggingT logger . withThreadContext ["app" .= app]

    (appSqlPool, appAWSEnv) <-
        runLogging
        $ (,)
        <$> createConnectionPool appDatabaseConf appStatementTimeout
        <*> AWS.discover

    runLogging $ runResourceT $ runReaderT f App { .. }

seedDB
    :: ( MonadUnliftIO m
       , MonadResource m
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

    seedJob demo 1 now (Just 127) invalidArgumentOutput
    seedJob restyled 1 now (Just 0) noDifferencesOutput
    seedJob restyler 1 now Nothing restylingOutput
    seedJob restyler 2 now (Just 10) configErrorOutput1

    Entity discountPlanId _ <- findOrCreateMarketplacePlan MarketplacePlan
        { marketplacePlanGithubId = Nothing
        , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceUnlimited
        , marketplacePlanName = "Friends & Family"
        , marketplacePlanDescription = "Manually managed discount plan"
        , marketplacePlanMonthlyRevenue = fromCents 0
        , marketplacePlanRetired = False
        }

    void $ upsert
        Offer
            { offerName = "Friends & Family"
            , offerDetails = "Exclusive offer for Friends of the Po- Restyled."
            , offerPurchaseUrl = "https://example.com"
            , offerMarketplacePlan = discountPlanId
            }
        [OfferMarketplacePlan =. discountPlanId]

    void $ upsert
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
restyledRepo name = Repo
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
restyledRepoPrivate name = (restyledRepo name) { repoIsPrivate = True }

seedJob
    :: ( MonadUnliftIO m
       , MonadResource m
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
    jobId <- insert $ Job
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
    let jobLogLines = uncurry jobLogLine <$> timestamped

    lift $ seedJobLogLines jobId jobLogLines

    -- Complete it if appropriate
    for_ mExitCode $ \ec -> do
        let lastLogLineAt = jobLogLineCreatedAt $ NE.last jobLogLines
            completedAt = addUTCTime 1 lastLogLineAt

        update
            jobId
            [ JobUpdatedAt =. completedAt
            , JobCompletedAt =. Just completedAt
            , JobExitCode =. Just ec
            ]
  where
    timestamped =
        NE.zip (secondsFrom createdAt) $ "Restyling..." :| untimestamped

secondsFrom :: UTCTime -> NonEmpty UTCTime
secondsFrom = NE.iterate $ addUTCTime 1

-- brittany-next-binding --columns=250

restylingOutput :: [Text]
restylingOutput =
    [ "Switched to a new branch 'issue#87'"
    , "Branch 'issue#87' set up to track remote branch 'issue#87' from 'origin'."
    , "[Info] Restyling restyled-io/restyler#88"
    , "[Info] Restyled PR does not exist"
    , "Switched to a new branch 'issue#87-restyled'"
    , "[Info] Restyling \"app/Http/Controllers/Store.php\" via \"php-cs-fixer\""
    , "[Debug] Loaded config default from \"/code/.php_cs\"."
    , "Paths from configuration file have been overridden by paths provided as command arguments."
    , " "
    , "Fixed all files in 0.010 seconds, 10.000 MB memory used"
    , "[Info] Restyling \"app/Models/Example.php\" via \"php-cs-fixer\""
    ]

-- brittany-next-binding --columns=250

noDifferencesOutput :: [Text]
noDifferencesOutput =
    [ "Branch 'lucky/ela-skills-debugger' set up to track remote branch 'lucky/ela-skills-debugger' from 'origin'."
    , "Switched to a new branch 'lucky/ela-skills-debugger'"
    , "[Info] Restyling freckle/megarepo#8616"
    , "[Info] Restyled PR does not exist"
    , "Switched to a new branch 'lucky/ela-skills-debugger-restyled'"
    , "[Info] Setting status of no differences for 686fe0a"
    , "[Info] No style differences found"
    ]

-- brittany-next-binding --columns=250

invalidArgumentOutput :: [Text]
invalidArgumentOutput =
    [ "From https://github.com/restyled.io/demo"
    , " * [new branch]      release/1 -> release/1"
    , "Switched to a new branch 'trim-fixes'"
    , "Branch 'trim-fixes' set up to track remote branch 'trim-fixes' from 'origin'."
    , "[Info] Restyling restyled.io/demo#1"
    , "[Info] Restyled PR does not exist"
    , "Switched to a new branch 'elevator-trim-fixes-restyled'"
    , "[Error] Process unsuccessful (ExitFailure 127)"
    , "Command: docker"
    , "Arguments: [--rm, --net=none, ...]"
    , " "
    , " "
    , "  docker: command not found"
    , "    1:some/stack"
    , "    75:trace/there"
    , " "
    , "Please see https://google.com"
    , " "
    , "Please see"
    , " "
    , "  - https://google.com"
    , "  - https://google.com"
    , "  - https://google.com"
    ]

-- brittany-next-binding --columns=250

configErrorOutput1 :: [Text]
configErrorOutput1 =
    [ "[Info] Restyler starting"
    , "[Info] Restyling restyled-io/restyler#153"
    , "[Info] No existing Restyled PR"
    , "[Info] Cloning repository"
    , "Switched to a new branch 'fix'"
    , "Branch 'fix' set up to track remote branch 'fix' from 'origin'."
    , "Switched to a new branch 'fix-restyled'"
    , "[Error] We had trouble with your configuration:"
    , " "
    , "  Yaml parse exception:"
    , "  Aeson exception:"
    , "  Error in $.restylers[2]: - Unexpected key \"prettier\", must be one of"
    , "  [\"name\",\"image\",\"command\",\"arguments\",\"include\",\"interpreters\",\"supports_arg_sep\",\"supports_multiple_paths\",\"documentation\"]."
    , "  "
    , "  "
    , "  Did you intend to specify a full Restyler object, or do you have incorrect"
    , "  indentation for a named override?"
    , "  "
    , "  Original input:"
    , "  # Restyler Configuration"
    , "  #"
    , "  # Overall notes:"
    , "  #"
    , "  # - All keys are optional and default as shown"
    , "  # - The entire config can also be just a list of values, which will be"
    , "  #   interpreted as specifying the restylers key"
    , "  #"
    , "  ####"
    , "  "
    , "  # Do anything at all?"
    , "  enabled: true"
    , "  "
    , "  # Push the style fixes directly to the original PR"
    , "  #"
    , "  # This setting implies pull_requests: false for origin PRs, and has no effect on"
    , "  # forked PRs (since we can't push to those)."
    , "  #"
    , "  auto: false"
    , "  "
    , "  # Download remote files before restyling"
    , "  #"
    , "  # Example:"
    , "  #"
    , "  #   remote_files:"
    , "  #     - url: https://raw.github.com/.../hlint.yaml"
    , "  #       path: .hlint.yaml"
    , "  #"
    , "  # Files must be publicly accessible."
    , "  #"
    , "  remote_files: []"
    , "  "
    , "  # Open Restyle PRs?"
    , "  pull_requests: true"
    , "  "
    , "  # Leave comments on the original PR linking to the Restyle PR?"
    , "  comments: true"
    , "  "
    , "  # Set commit statuses on the original PR?"
    , "  statuses:"
    , "    # Red status in the case of differences found"
    , "    differences: true"
    , "    # Green status in the case of no differences found"
    , "    no_differences: true"
    , "    # Red status if we encounter errors restyling"
    , "    error: true"
    , "  "
    , "  # Request review on the Restyle PR?"
    , "  #"
    , "  # Possible values:"
    , "  #"
    , "  #   author: From the author of the original PR"
    , "  #   owner: From the owner of the repository"
    , "  #"
    , "  # One value will apply to origin and forked PRs, but you can also specify"
    , "  # separate values."
    , "  #"
    , "  request_review: none"
    , "  "
    , "  # Add labels to any created Restyle PRs"
    , "  #"
    , "  # These can be used to tell other automation to avoid our PRs."
    , "  #"
    , "  labels: []"
    , "  "
    , "  # Which restylers to run"
    , "  #"
    , "  # See restyled-io/restylers repository for their defaults."
    , "  #"
    , "  restylers:"
    , "    - black:"
    , "        arguments:"
    , "          - \"--line-length 100\""
    , "    - shfmt:"
    , "        arguments:"
    , "          - \"-i\""
    , "          - \"2\""
    , "          - \"-ci\""
    , "          - \"-bn\""
    , "          - \"-sr\""
    , "    - prettier:"
    , "      include:"
    , "        - \"**/*.js\""
    , "        - \"**/*.jsx\""
    , "        - \"**/*.yml\""
    , "        - \"**/*.yaml\""
    , "        - \"!chats/**/*\""
    , "        - \"!flow/**/*\""
    , "   "
    , "  # Version of the set of Restylers to run"
    , "  #"
    , "  # This should correspond to a ref on the restyled-io/restylers repository,"
    , "  # usually it's a tag that is a date of when that set was released. You could"
    , "  # re-specific the default in your own config if you prefer to avoid update"
    , "  # surprises."
    , "  #"
    , "  restylers_version: \"20190715\""
    , " "
    , "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml"
    , " "
    ]

seedJobLogLines
    :: ( MonadUnliftIO m
       , MonadResource m
       , MonadReader env m
       , HasSettings env
       , HasAWS env
       )
    => JobId
    -> NonEmpty JobLogLine
    -> m ()
seedJobLogLines jobId jobLogLines = do
    AppSettings {..} <- view settingsL

    let logGroup = appRestylerLogGroup
        logStream = appRestylerLogStreamPrefix <> toPathPiece jobId
        events = toInputLogEvent <$> jobLogLines

    ignoring _ResourceNotFoundException $ void $ AWS.send $ newDeleteLogStream
        logGroup
        logStream

    void $ AWS.send $ newCreateLogStream logGroup logStream
    void $ AWS.send $ newPutLogEvents logGroup logStream events

toInputLogEvent :: JobLogLine -> InputLogEvent
toInputLogEvent ln = newInputLogEvent
    (utcTimeToPOSIXMilliseconds $ jobLogLineCreatedAt ln)
    (jobLogLineContent ln)

utcTimeToPOSIXMilliseconds :: Integral n => UTCTime -> n
utcTimeToPOSIXMilliseconds = round . (* 1000) . utcTimeToPOSIXSeconds

ignoring
    :: MonadUnliftIO m => Getting (First a) SomeException a -> m () -> m ()
ignoring l f = catching_ l f $ pure ()

catching_
    :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)

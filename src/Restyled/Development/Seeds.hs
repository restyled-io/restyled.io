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
import Restyled.AWS (HasAWS (..))
import qualified Restyled.AWS as AWS
import Restyled.DB
import Restyled.Env
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
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
  seedJob restyled 2 now (Just 0) restylingOutputGHA
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
  let jobLogLines = uncurry jobLogLine <$> timestamped

  lift $ seedJobLogLines jobId jobLogLines

  -- Complete it if appropriate
  for_ mExitCode $ \ec -> do
    let
      lastLogLineAt = jobLogLineCreatedAt $ NE.last jobLogLines
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

restylingOutputGHA :: [Text]
restylingOutputGHA =
  [ "time=\"2024-07-23T18:02:43Z\" level=info msg=\"Using docker host 'unix:///var/run/docker.sock', and daemon socket 'unix:///var/run/docker.sock'\""
  , "time=\"2024-07-23T18:02:43Z\" level=error msg=\"path/restyler-1rGn3dnot located inside a git repository\" error=\"repository does not exist\""
  , "time=\"2024-07-23T18:02:43Z\" level=warning msg=\"unable to get git revision: repository does not exist\""
  , "[Agent/restyled] 🚀  Start image=catthehacker/ubuntu:act-latest"
  , "[Agent/restyled]   🐳  docker pull image=catthehacker/ubuntu:act-latest platform= username= forcePull=true"
  , "[Agent/restyled]   🐳  docker create image=catthehacker/ubuntu:act-latest platform= entrypoint=[\"tail\" \"-f\" \"/dev/null\"] cmd=[] network=\"host\""
  , "[Agent/restyled]   🐳  docker run image=catthehacker/ubuntu:act-latest platform= entrypoint=[\"tail\" \"-f\" \"/dev/null\"] cmd=[] network=\"host\""
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ☁  git clone 'https://github.com/actions/checkout' # ref=v4"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ☁  git clone 'https://github.com/restyled-io/restyler' # ref=main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Pre restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ✅  Success - Pre restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ☁  git clone 'https://github.com/restyled-io/restyler' # ref=main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Pre restyled-io/restyler/actions/run@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ✅  Success - Pre restyled-io/restyler/actions/run@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ☁  git clone 'https://github.com/peter-evans/create-pull-request' # ref=v6"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main actions/checkout@v4"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/actions-checkout@v4/ dst=/var/run/act/actions/actions-checkout@v4/"
  , "[Agent/restyled]   🐳  docker exec cmd=[node /var/run/act/actions/actions-checkout@v4/dist/index.js] user= workdir="
  , "[Agent/restyled]   💬  ::debug::GITHUB_WORKSPACE = '/restyler-1rGn3d'"
  , "[Agent/restyled]   💬  ::debug::qualified repository = 'restyled-io/restyled.io'"
  , "[Agent/restyled]   💬  ::debug::ref = 'pb/gha'"
  , "[Agent/restyled]   💬  ::debug::commit = 'undefined'"
  , "[Agent/restyled]   💬  ::debug::clean = true"
  , "[Agent/restyled]   💬  ::debug::filter = undefined"
  , "[Agent/restyled]   💬  ::debug::fetch depth = 1"
  , "[Agent/restyled]   💬  ::debug::fetch tags = false"
  , "[Agent/restyled]   💬  ::debug::show progress = true"
  , "[Agent/restyled]   💬  ::debug::lfs = false"
  , "[Agent/restyled]   💬  ::debug::submodules = false"
  , "[Agent/restyled]   💬  ::debug::recursive submodules = false"
  , "[Agent/restyled]   💬  ::debug::Repository owner ID not found within GITHUB event info"
  , "[Agent/restyled]   💬  ::debug::GitHub Host URL = "
  , "[Agent/restyled]   ❓ add-matcher /run/act/actions/actions-checkout@v4/dist/problem-matcher.json"
  , "[Agent/restyled]   | Syncing repository: restyled-io/restyled.io"
  , "[Agent/restyled]   ❓  ::group::Getting Git version info"
  , "[Agent/restyled]   | Working directory is '/restyler-1rGn3d'"
  , "[Agent/restyled]   💬  ::debug::Getting git version"
  , "[Agent/restyled]   | [command]/usr/bin/git version"
  , "[Agent/restyled]   | git version 2.45.2"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::git version 2.45.2%0A"
  , "[Agent/restyled]   💬  ::debug::Set git useragent to: git/2.45.2 (github-actions-checkout)"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ⚙  ***"
  , "[Agent/restyled]   | Temporarily overriding HOME='/tmp/bcf94eaf-5ac7-44f9-b99d-92f9c055d960' before making global git config changes"
  , "[Agent/restyled]   | Adding repository directory to the temporary git global config as a safe directory"
  , "[Agent/restyled]   | [command]/usr/bin/git config --global --add safe.directory /restyler-1rGn3d"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | Deleting the contents of '/restyler-1rGn3d'"
  , "[Agent/restyled]   ❓  ::group::Initializing the repository"
  , "[Agent/restyled]   | [command]/usr/bin/git init /restyler-1rGn3d"
  , "[Agent/restyled]   | hint: Using 'master' as the name for the initial branch. This default branch name"
  , "[Agent/restyled]   | hint: is subject to change. To configure the initial branch name to use in all"
  , "[Agent/restyled]   | hint: of your new repositories, which will suppress this warning, call:"
  , "[Agent/restyled]   | hint:"
  , "[Agent/restyled]   | hint: \tgit config --global init.defaultBranch <name>"
  , "[Agent/restyled]   | hint:"
  , "[Agent/restyled]   | hint: Names commonly chosen instead of 'master' are 'main', 'trunk' and"
  , "[Agent/restyled]   | hint: 'development'. The just-created branch can be renamed via this command:"
  , "[Agent/restyled]   | hint:"
  , "[Agent/restyled]   | hint: \tgit branch -m <name>"
  , "[Agent/restyled]   | Initialized empty Git repository in /restyler-1rGn3d/.git/"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::Initialized empty Git repository in /restyler-1rGn3d/.git/%0A"
  , "[Agent/restyled]   | [command]/usr/bin/git remote add origin https://github.com/restyled-io/restyled.io"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Disabling automatic garbage collection"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local gc.auto 0"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Setting up auth"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp core\\.sshCommand"
  , "[Agent/restyled]   💬  ::debug::1"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git submodule foreach --recursive sh -c \"git config --local --name-only --get-regexp 'core\\.sshCommand' && git config --local --unset-all 'core.sshCommand' || :\""
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp http\\.https\\:\\/\\/github\\.com\\/\\.extraheader"
  , "[Agent/restyled]   💬  ::debug::1"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git submodule foreach --recursive sh -c \"git config --local --name-only --get-regexp 'http\\.https\\:\\/\\/github\\.com\\/\\.extraheader' && git config --local --unset-all 'http.https://github.com/.extraheader' || :\""
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local http.https://github.com/.extraheader AUTHORIZATION: basic ***"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Fetching the repository"
  , "[Agent/restyled]   | [command]/usr/bin/git -c protocol.version=2 fetch --no-tags --prune --no-recurse-submodules --depth=1 origin +refs/heads/pb/gha*:refs/remotes/origin/pb/gha* +refs/tags/pb/gha*:refs/tags/pb/gha*"
  , "[Agent/restyled]   | From https://github.com/restyled-io/restyled.io"
  , "[Agent/restyled]   |  * [new branch]      pb/gha     -> origin/pb/gha"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Determining the checkout info"
  , "[Agent/restyled]   | [command]/usr/bin/git branch --list --remote origin/pb/gha"
  , "[Agent/restyled]   |   origin/pb/gha"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::  origin/pb/gha%0A"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   | [command]/usr/bin/git sparse-checkout disable"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --unset-all extensions.worktreeConfig"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   ❓  ::group::Checking out the ref"
  , "[Agent/restyled]   | [command]/usr/bin/git checkout --progress --force -B pb/gha refs/remotes/origin/pb/gha"
  , "[Agent/restyled]   | Switched to a new branch 'pb/gha'"
  , "[Agent/restyled]   | branch 'pb/gha' set up to track 'origin/pb/gha'."
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::branch 'pb/gha' set up to track 'origin/pb/gha'.%0A"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::commit 04a8a3a5ea62ae521047f2dc76132e3483c00320%0AAuthor: patrick brisbin <pbrisbin@gmail.com>%0ADate:   Tue Jul 23 14:02:30 2024 -0400%0A%0A    Fix image-check in justfile%0A"
  , "[Agent/restyled]   | [command]/usr/bin/git log -1 --format='%H'"
  , "[Agent/restyled]   | '04a8a3a5ea62ae521047f2dc76132e3483c00320'"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::'04a8a3a5ea62ae521047f2dc76132e3483c00320'%0A"
  , "[Agent/restyled]   💬  ::debug::Unsetting HOME override"
  , "[Agent/restyled]   ❓  ::remove-matcher owner=checkout-git::"
  , "[Agent/restyled]   ✅  Success - Main actions/checkout@v4"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/restyled-io-restyler-actions-setup@main/ dst=/var/run/act/actions/restyled-io-restyler-actions-setup@main/"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/restyled-io/restyler/main/install |"
  , "  sudo sh -s -- -t \"$TAG\""
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker exec cmd=[bash --noprofile --norc -e -o pipefail /var/run/act/workflow/1-composite-0.sh] user= workdir=/tmp"
  , "[Agent/restyled]   | Downloading v0.3.2.0/restyler-linux-x86_64..."
  , "[Agent/restyled]   | Installing binaries in /usr/local/bin"
  , "[Agent/restyled]   | 'restyler-linux-x86_64/restyle' -> '/usr/local/bin/restyle'"
  , "[Agent/restyled]   | 'restyler-linux-x86_64/restyle-gha' -> '/usr/local/bin/restyle-gha'"
  , "[Agent/restyled]   | Cleaning up..."
  , "[Agent/restyled]   ✅  Success - Main curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/restyled-io/restyler/main/install |"
  , "  sudo sh -s -- -t \"$TAG\""
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ✅  Success - Main restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main restyled-io/restyler/actions/run@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/restyled-io-restyler-actions-run@main/ dst=/var/run/act/actions/restyled-io-restyler-actions-run@main/"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main restyle-gha --pr 'restyled-io/restyled.io#311'"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker exec cmd=[bash --noprofile --norc -e -o pipefail /var/run/act/workflow/restyler-composite-restyler.sh] user= workdir="
  , "[Agent/restyled]   | {\"time\":\"2024-07-23T18:57:43.567218004Z\",\"level\":\"info\",\"location\":{\"package\":\"restyler-0.3.0.0-777FApzYHe9I9GuELxSPdI\",\"module\":\"Restyler.GHA\",\"file\":\"src/Restyler/GHA.hs\",\"line\":45,\"char\":3},\"message\":{\"text\":\"Handling PR\",\"meta\":{\"owner\":\"restyled-io\",\"repo\":\"restyled.io\",\"number\":311,\"state\":\"open\",\"title\":\"WIP: start on justfile\",\"base\":\"main\",\"head\":\"pb/gha\"}}}"
  , "[Agent/restyled]   | docker.io/restyled/restyler-whitespace:v0.2.0.0"
  , "[Agent/restyled]   | {\"time\":\"2024-07-23T18:57:44.517177389Z\",\"level\":\"info\",\"location\":{\"package\":\"restyler-0.3.0.0-777FApzYHe9I9GuELxSPdI\",\"module\":\"Restyler.Restyler.Run\",\"file\":\"src/Restyler/Restyler/Run.hs\",\"line\":326,\"char\":7},\"message\":{\"text\":\"Running whitespace on justfile\"}}"
  , "[Agent/restyled]   | [pb/agent-workflow c39d924] Restyled by whitespace"
  , "[Agent/restyled]   |  2 files changed, 9 insertions(+), 2 deletions(-)"
  , "[Agent/restyled]   | {\"time\":\"2024-07-23T18:57:45.331035865Z\",\"level\":\"info\",\"location\":{\"package\":\"restyler-0.3.0.0-777FApzYHe9I9GuELxSPdI\",\"module\":\"Restyler.CLI\",\"file\":\"src/Restyler/CLI.hs\",\"line\":31,\"char\":20},\"message\":{\"text\":\"Differences found\"}}"
  , "[Agent/restyled]   ✅  Success - Main restyle-gha --pr 'restyled-io/restyled.io#311'"
  , "[Agent/restyled]   ⚙  ::set-output:: differences=false"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-base=pb/gha"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-head=restyled/pb/gha"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-title="
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-labels="
  , "[Agent/restyled]   ⚙  ::set-output:: differences=false"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-base=pb/gha"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-reviewers="
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-team-reviewers="
  , "[Agent/restyled]   ⚙  ::set-output:: skipped="
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-head=restyled/pb/gha"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-body="
  , "[Agent/restyled]   ✅  Success - Main restyled-io/restyler/actions/run@main"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-base=pb/gha"
  , "[Agent/restyled]   ⚙  ::set-output:: restyled-head=restyled/pb/gha"
  , "[Agent/restyled]   ⚙  ::set-output:: differences=false"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Main peter-evans/create-pull-request@v6"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/peter-evans-create-pull-request@v6/ dst=/var/run/act/actions/peter-evans-create-pull-request@v6/"
  , "[Agent/restyled]   🐳  docker exec cmd=[node /var/run/act/actions/peter-evans-create-pull-request@v6/dist/index.js] user= workdir="
  , "[Agent/restyled]   💬  ::debug::Inputs: {%0A  token: '***',%0A  gitToken: '',%0A  path: '',%0A  addPaths: [],%0A  commitMessage: '[create-pull-request] automated change',%0A  committer: 'github-actions[bot] <41898282+github-actions[bot]@users.noreply.github.com>',%0A  author: 'nektos/act <+nektos/act@users.noreply.github.com>',%0A  signoff: false,%0A  branch: 'restyled/pb/gha',%0A  deleteBranch: true,%0A  branchSuffix: '',%0A  base: 'pb/gha',%0A  pushToFork: '',%0A  title: '',%0A  body: '',%0A  bodyPath: '',%0A  labels: [],%0A  assignees: [],%0A  reviewers: [],%0A  teamReviewers: [],%0A  milestone: 0,%0A  draft: false%0A}"
  , "[Agent/restyled]   ❓  ::group::Prepare git configuration"
  , "[Agent/restyled]   💬  ::debug::githubWorkspacePath: /restyler-1rGn3d"
  , "[Agent/restyled]   💬  ::debug::repoPath: /restyler-1rGn3d"
  , "[Agent/restyled]   | [command]/usr/bin/git config --global --name-only --get-regexp safe.directory /restyler-1rGn3d"
  , "[Agent/restyled]   | [command]/usr/bin/git config --global --add safe.directory /restyler-1rGn3d"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --get remote.origin.url"
  , "[Agent/restyled]   | https://github.com/restyled-io/restyled.io"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | http.https://github.com/.extraheader"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --get-regexp http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | http.https://github.com/.extraheader AUTHORIZATION: basic ***"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --unset http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | Unset config key 'http.https://github.com/.extraheader'"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Determining the base and head repositories"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   | Pull request branch target repository set to restyled-io/restyled.io"
  , "[Agent/restyled]   ❓  ::group::Configuring credential for HTTPS authentication"
  , "[Agent/restyled]   ⚙  ***"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local http.https://github.com/.extraheader AUTHORIZATION: basic ***"
  , "[Agent/restyled]   | [command]/usr/bin/git rev-parse --git-dir"
  , "[Agent/restyled]   | .git"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Checking the base repository state"
  , "[Agent/restyled]   | [command]/usr/bin/git symbolic-ref HEAD --short"
  , "[Agent/restyled]   | pb/gha"
  , "[Agent/restyled]   | Working base is branch 'pb/gha'"
  , "[Agent/restyled]   | [command]/usr/bin/git remote prune origin"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   | Pull request branch to create or update set to 'restyled/pb/gha'"
  , "[Agent/restyled]   ❓  ::group::Configuring the committer and author"
  , "[Agent/restyled]   | Configured git committer as 'github-actions[bot] <41898282+github-actions[bot]@users.noreply.github.com>'"
  , "[Agent/restyled]   | Configured git author as 'nektos/act <+nektos/act@users.noreply.github.com>'"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Create or update the pull request branch"
  , "[Agent/restyled]   | [command]/usr/bin/git symbolic-ref HEAD --short"
  , "[Agent/restyled]   | pb/gha"
  , "[Agent/restyled]   | Working base is branch 'pb/gha'"
  , "[Agent/restyled]   | [command]/usr/bin/git checkout --progress -B d0ea29d2-d64d-41ec-ba6a-ec8214dbb25e HEAD --"
  , "[Agent/restyled]   | Switched to a new branch 'd0ea29d2-d64d-41ec-ba6a-ec8214dbb25e'"
  , "[Agent/restyled]   | [command]/usr/bin/git status --porcelain -unormal --"
  , "[Agent/restyled]   | [command]/usr/bin/git diff --quiet --"
  , "[Agent/restyled]   | [command]/usr/bin/git diff --quiet --staged --"
  , "[Agent/restyled]   | [command]/usr/bin/git stash push --include-untracked"
  , "[Agent/restyled]   | No local changes to save"
  , "[Agent/restyled]   | Resetting working base branch 'pb/gha'"
  , "[Agent/restyled]   | [command]/usr/bin/git checkout --progress pb/gha --"
  , "[Agent/restyled]   | Switched to branch 'pb/gha'"
  , "[Agent/restyled]   | Your branch is up to date with 'origin/pb/gha'."
  , "[Agent/restyled]   | [command]/usr/bin/git reset --hard origin/pb/gha"
  , "[Agent/restyled]   | HEAD is now at 04a8a3a Fix image-check in justfile"
  , "[Agent/restyled]   | [command]/usr/bin/git rev-list --right-only --count pb/gha...d0ea29d2-d64d-41ec-ba6a-ec8214dbb25e"
  , "[Agent/restyled]   | 0"
  , "[Agent/restyled]   | [command]/usr/bin/git -c protocol.version=2 fetch --no-tags --progress --no-recurse-submodules --force --depth=10 origin restyled/pb/gha:refs/remotes/origin/restyled/pb/gha"
  , "[Agent/restyled]   | fatal: couldn't find remote ref restyled/pb/gha"
  , "[Agent/restyled]   | Pull request branch 'restyled/pb/gha' does not exist yet."
  , "[Agent/restyled]   | [command]/usr/bin/git checkout --progress -B restyled/pb/gha d0ea29d2-d64d-41ec-ba6a-ec8214dbb25e --"
  , "[Agent/restyled]   | Switched to a new branch 'restyled/pb/gha'"
  , "[Agent/restyled]   | [command]/usr/bin/git rev-list --right-only --count pb/gha...restyled/pb/gha"
  , "[Agent/restyled]   | 0"
  , "[Agent/restyled]   | Branch 'restyled/pb/gha' is not ahead of base 'pb/gha' and will not be created"
  , "[Agent/restyled]   | [command]/usr/bin/git rev-parse HEAD"
  , "[Agent/restyled]   | 04a8a3a5ea62ae521047f2dc76132e3483c00320"
  , "[Agent/restyled]   | [command]/usr/bin/git branch --delete --force d0ea29d2-d64d-41ec-ba6a-ec8214dbb25e"
  , "[Agent/restyled]   | Deleted branch d0ea29d2-d64d-41ec-ba6a-ec8214dbb25e (was 04a8a3a)."
  , "[Agent/restyled]   | [command]/usr/bin/git checkout --progress pb/gha --"
  , "[Agent/restyled]   | Switched to branch 'pb/gha'"
  , "[Agent/restyled]   | Your branch is up to date with 'origin/pb/gha'."
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ❓  ::group::Restore git configuration"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | http.https://github.com/.extraheader"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --get-regexp http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | http.https://github.com/.extraheader AUTHORIZATION: basic ***"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --unset http.https://github.com/.extraheader ^AUTHORIZATION:"
  , "[Agent/restyled]   | Unset config key 'http.https://github.com/.extraheader'"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local http.https://github.com/.extraheader AUTHORIZATION: basic ***"
  , "[Agent/restyled]   | Persisted git credentials restored"
  , "[Agent/restyled]   | [command]/usr/bin/git config --global --unset safe.directory /restyler-1rGn3d"
  , "[Agent/restyled]   ❓  ::endgroup::"
  , "[Agent/restyled]   ✅  Success - Main peter-evans/create-pull-request@v6"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Post restyled-io/restyler/actions/run@main"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/restyled-io-restyler-actions-run@main/ dst=/var/run/act/actions/restyled-io-restyler-actions-run@main/"
  , "[Agent/restyled]   ✅  Success - Post restyled-io/restyler/actions/run@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Post restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled]   🐳  docker cp src=/root/.cache/act/restyled-io-restyler-actions-setup@main/ dst=/var/run/act/actions/restyled-io-restyler-actions-setup@main/"
  , "[Agent/restyled]   ✅  Success - Post restyled-io/restyler/actions/setup@main"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled] ⭐ Run Post actions/checkout@v4"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
  , "[Agent/restyled]   🐳  docker exec cmd=[node /var/run/act/actions/actions-checkout@v4/dist/index.js] user= workdir="
  , "[Agent/restyled]   💬  ::debug::Getting git version"
  , "[Agent/restyled]   | [command]/usr/bin/git version"
  , "[Agent/restyled]   | git version 2.45.2"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::git version 2.45.2%0A"
  , "[Agent/restyled]   💬  ::debug::Set git useragent to: git/2.45.2 (github-actions-checkout)"
  , "[Agent/restyled]   ⚙  ***"
  , "[Agent/restyled]   | Copying '/root/.gitconfig' to '/tmp/d039481f-eb27-42d2-9e51-a54139c31995/.gitconfig'"
  , "[Agent/restyled]   | Temporarily overriding HOME='/tmp/d039481f-eb27-42d2-9e51-a54139c31995' before making global git config changes"
  , "[Agent/restyled]   | Adding repository directory to the temporary git global config as a safe directory"
  , "[Agent/restyled]   | [command]/usr/bin/git config --global --add safe.directory /restyler-1rGn3d"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp core\\.sshCommand"
  , "[Agent/restyled]   💬  ::debug::1"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git submodule foreach --recursive sh -c \"git config --local --name-only --get-regexp 'core\\.sshCommand' && git config --local --unset-all 'core.sshCommand' || :\""
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --name-only --get-regexp http\\.https\\:\\/\\/github\\.com\\/\\.extraheader"
  , "[Agent/restyled]   | http.https://github.com/.extraheader"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::http.https://github.com/.extraheader%0A"
  , "[Agent/restyled]   | [command]/usr/bin/git config --local --unset-all http.https://github.com/.extraheader"
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   | [command]/usr/bin/git submodule foreach --recursive sh -c \"git config --local --name-only --get-regexp 'http\\.https\\:\\/\\/github\\.com\\/\\.extraheader' && git config --local --unset-all 'http.https://github.com/.extraheader' || :\""
  , "[Agent/restyled]   💬  ::debug::0"
  , "[Agent/restyled]   💬  ::debug::"
  , "[Agent/restyled]   💬  ::debug::Unsetting HOME override"
  , "[Agent/restyled]   ✅  Success - Post actions/checkout@v4"
  , "[Agent/restyled] Cleaning up container for job restyled"
  , "[Agent/restyled] 🏁  Job succeeded"
  , "[Agent/restyled] path/restyler-1rGn3dnot located inside a git repository"
  , "[Agent/restyled] unable to get git revision: repository does not exist"
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

  let
    logGroup = appRestylerLogGroup
    logStream = appRestylerLogStreamPrefix <> toPathPiece jobId
    events = toInputLogEvent <$> jobLogLines

  ignoring _ResourceNotFoundException
    $ void
    $ AWS.send
    $ newDeleteLogStream
      logGroup
      logStream

  void $ AWS.send $ newCreateLogStream logGroup logStream
  void $ AWS.send $ newPutLogEvents logGroup logStream events

toInputLogEvent :: JobLogLine -> InputLogEvent
toInputLogEvent ln =
  newInputLogEvent
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

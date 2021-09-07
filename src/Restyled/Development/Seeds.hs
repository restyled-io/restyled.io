module Restyled.Development.Seeds
    ( seedDB
    ) where

import Restyled.Prelude

import qualified Prelude as Unsafe
import Restyled.JobLogLine
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Settings

seedDB
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => SqlPersistT m ()
seedDB = do
    now <- liftIO getCurrentTime

    Entity _ demo <- upsertRepo $ restyledRepo "demo"
    Entity _ restyled <- upsertRepo $ restyledRepo "restyled.io"
    Entity _ restyler <- upsertRepo $ restyledRepo "restyler"
    Entity _ _ops <- upsertRepo $ restyledRepoPrivate "ops"

    -- We don't seed an enabled example because for it to be functional would
    -- mean having secrets in the seeds, and we'd rather not have a
    -- non-functional machine in the seeded database.
    void $ upsert
        RestyleMachine
            { restyleMachineName = "disabled-example"
            , restyleMachineEnabled = False
            , restyleMachineHost = "tcp://123.123.123:123"
            , restyleMachineCaCert = "-- CA --\n"
            , restyleMachineCert = "-- CERT --\n"
            , restyleMachineKey = "-- Key --\n"
            , restyleMachineJobCount = 0
            , restyleMachineReconciling = False
            }
        []

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
    :: (MonadUnliftIO m, MonadReader env m, HasSettings env, HasAWS env)
    => Repo
    -> PullRequestNum
    -> UTCTime
    -> Maybe Int
    -> [Text]
    -> SqlPersistT m ()
seedJob Repo {..} pullRequest createdAt mExitCode untimestamped = do
    -- Start the Job
    jobId <- insert $ markJobAsCloudWatch $ Job
        { jobSvcs = repoSvcs
        , jobOwner = repoOwner
        , jobRepo = repoName
        , jobPullRequest = pullRequest
        , jobCreatedAt = createdAt
        , jobUpdatedAt = createdAt
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobLog = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

    -- Capture it "running"
    let
        jobLogLines = map
            (\(t, content) -> JobLogLine
                { jobLogLineJob = jobId
                , jobLogLineCreatedAt = t
                , jobLogLineStream = "system"
                , jobLogLineContent = content
                }
            )
            timestamped

    lift $ do
        deleteJobLogLines jobId
        captureJobLogLines jobLogLines

    -- Complete it if appropriate
    for_ mExitCode $ \ec -> do
        let lastLogLineAt = jobLogLineCreatedAt $ Unsafe.last jobLogLines
            completedAt = addUTCTime 1 lastLogLineAt

        update
            jobId
            [ JobUpdatedAt =. completedAt
            , JobCompletedAt =. Just completedAt
            , JobExitCode =. Just ec
            ]
  where
    timestamped =
        zip (secondsFrom createdAt)
            $ "docker run --rm ..."
            : "Running on ..."
            : untimestamped

secondsFrom :: UTCTime -> [UTCTime]
secondsFrom t0 = let t1 = addUTCTime 1 t0 in t1 : secondsFrom t1

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

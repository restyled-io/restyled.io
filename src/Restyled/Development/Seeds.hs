module Restyled.Development.Seeds
    ( seedDB
    ) where

import Restyled.Prelude

import qualified Data.List.NonEmpty as NE
import Restyled.JobLogLine
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Settings

seedDB
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
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
    :: (MonadIO m, MonadReader env m, HasSettings env, HasAWS env)
    => Repo
    -> PullRequestNum
    -> UTCTime
    -> Maybe Int
    -> [(JobLogStream, Text)]
    -> SqlPersistT m ()
seedJob Repo {..} pullRequest createdAt mExitCode untimestamped = do
    -- Start the Job
    jobId <- insert Job
        { jobSvcs = repoSvcs
        , jobOwner = repoOwner
        , jobRepo = repoName
        , jobPullRequest = pullRequest
        , jobCreatedAt = createdAt
        , jobUpdatedAt = createdAt
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobLog = Nothing
        , jobStdout = Just "__cw"
        , jobStderr = Nothing
        }

    -- Capture it "running"
    lift
        $ captureJobLogLines jobId
        $ (\(t, (stream, content)) -> jobLogLineAsOf t jobId stream content)
        <$> timestamped

    -- Complete it if appropriate
    for_ mExitCode $ \ec -> do
        let lastLogLineAt = fst $ NE.last timestamped
            completedAt = addUTCTime 1 lastLogLineAt

        lift
            $ captureJobLogLines jobId
            $ pure
            $ jobLogLineAsOf completedAt jobId JobLogStreamSystem
            $ "Restyler exited "
            <> tshow ec

        update
            jobId
            [ JobUpdatedAt =. completedAt
            , JobCompletedAt =. Just completedAt
            , JobExitCode =. Just ec
            ]
  where
    timestamped =
        NE.fromList
            $ zip (secondsFrom createdAt)
            $ (JobLogStreamSystem, "docker run --rm ...")
            : (JobLogStreamSystem, "Running on ...")
            : untimestamped

secondsFrom :: UTCTime -> [UTCTime]
secondsFrom t0 = let t1 = addUTCTime 1 t0 in t1 : secondsFrom t1

-- brittany-next-binding --columns=250

restylingOutput :: [(JobLogStream, Text)]
restylingOutput =
    [ (JobLogStreamStderr, "Switched to a new branch 'issue#87'")
    , (JobLogStreamStdout, "Branch 'issue#87' set up to track remote branch 'issue#87' from 'origin'.")
    , (JobLogStreamStdout, "Restyling restyled-io/restyler#88")
    , (JobLogStreamStdout, "Restyled PR does not exist")
    , (JobLogStreamStderr, "Switched to a new branch 'issue#87-restyled'")
    , (JobLogStreamStdout, "Restyling \"app/Http/Controllers/Store.php\" via \"php-cs-fixer\"")
    , (JobLogStreamStderr, "Loaded config default from \"/code/.php_cs\".")
    , (JobLogStreamStderr, "Paths from configuration file have been overridden by paths provided as command arguments.")
    , (JobLogStreamStdout, "Fixed all files in 0.010 seconds, 10.000 MB memory used")
    , (JobLogStreamStdout, "Restyling \"app/Models/Example.php\" via \"php-cs-fixer\"")
    ]

-- brittany-next-binding --columns=250

noDifferencesOutput :: [(JobLogStream, Text)]
noDifferencesOutput =
    [ (JobLogStreamStdout, "Branch 'lucky/ela-skills-debugger' set up to track remote branch 'lucky/ela-skills-debugger' from 'origin'.")
    , (JobLogStreamStderr, "Switched to a new branch 'lucky/ela-skills-debugger'")
    , (JobLogStreamStdout, "Restyling freckle/megarepo#8616")
    , (JobLogStreamStdout, "Restyled PR does not exist")
    , (JobLogStreamStderr, "Switched to a new branch 'lucky/ela-skills-debugger-restyled'")
    , (JobLogStreamStdout, "Setting status of no differences for 686fe0a")
    , (JobLogStreamStdout, "No style differences found")
    ]

-- brittany-next-binding --columns=250

invalidArgumentOutput :: [(JobLogStream, Text)]
invalidArgumentOutput =
    [ (JobLogStreamStderr, "From https://github.com/restyled.io/demo")
    , (JobLogStreamStderr, " * [new branch]      release/1 -> release/1")
    , (JobLogStreamStderr, "Switched to a new branch 'trim-fixes'")
    , (JobLogStreamStdout, "Branch 'trim-fixes' set up to track remote branch 'trim-fixes' from 'origin'.")
    , (JobLogStreamStdout, "Restyling restyled.io/demo#1")
    , (JobLogStreamStdout, "Restyled PR does not exist")
    , (JobLogStreamStderr, "Switched to a new branch 'elevator-trim-fixes-restyled'")
    , (JobLogStreamStderr, "Process unsuccessful (ExitFailure 127)")
    , (JobLogStreamStderr, "Command: docker")
    , (JobLogStreamStderr, "Arguments: [--rm, --net=none, ...]")
    , (JobLogStreamStderr, "  docker: command not found")
    , (JobLogStreamStderr, "    1:some/stack")
    , (JobLogStreamStderr, "    75:trace/there")
    , (JobLogStreamStderr, "Please see https://google.com")
    , (JobLogStreamStderr, "Please see")
    , (JobLogStreamStderr, "  - https://google.com")
    , (JobLogStreamStderr, "  - https://google.com")
    , (JobLogStreamStderr, "  - https://google.com")
    ]

-- brittany-next-binding --columns=250

configErrorOutput1 :: [(JobLogStream, Text)]
configErrorOutput1 =
    [ (JobLogStreamStderr, "Switched to a new branch 'fix'")
    , (JobLogStreamStdout, "Branch 'fix' set up to track remote branch 'fix' from 'origin'.")
    , (JobLogStreamStderr, "Switched to a new branch 'fix-restyled'")
    , (JobLogStreamStderr, "We had trouble with your configuration:")
    , (JobLogStreamStderr, "  Yaml parse exception:")
    , (JobLogStreamStderr, "  Aeson exception:")
    , (JobLogStreamStderr, "  Error in $.restylers[2]: - Unexpected key \"prettier\", must be one of")
    , (JobLogStreamStderr, "  [\"name\",\"image\",\"command\",\"arguments\",\"include\",\"interpreters\",\"supports_arg_sep\",\"supports_multiple_paths\",\"documentation\"].")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  Did you intend to specify a full Restyler object, or do you have incorrect")
    , (JobLogStreamStderr, "  indentation for a named override?")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  Original input:")
    , (JobLogStreamStderr, "  # Restyler Configuration")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # Overall notes:")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # - All keys are optional and default as shown")
    , (JobLogStreamStderr, "  # - The entire config can also be just a list of values, which will be")
    , (JobLogStreamStderr, "  #   interpreted as specifying the restylers key")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  ####")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Do anything at all?")
    , (JobLogStreamStderr, "  enabled: true")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Push the style fixes directly to the original PR")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # This setting implies pull_requests: false for origin PRs, and has no effect on")
    , (JobLogStreamStderr, "  # forked PRs (since we can't push to those).")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  auto: false")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Download remote files before restyling")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # Example:")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  #   remote_files:")
    , (JobLogStreamStderr, "  #     - url: https://raw.github.com/.../hlint.yaml")
    , (JobLogStreamStderr, "  #       path: .hlint.yaml")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # Files must be publicly accessible.")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  remote_files: []")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Open Restyle PRs?")
    , (JobLogStreamStderr, "  pull_requests: true")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Leave comments on the original PR linking to the Restyle PR?")
    , (JobLogStreamStderr, "  comments: true")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Set commit statuses on the original PR?")
    , (JobLogStreamStderr, "  statuses:")
    , (JobLogStreamStderr, "    # Red status in the case of differences found")
    , (JobLogStreamStderr, "    differences: true")
    , (JobLogStreamStderr, "    # Green status in the case of no differences found")
    , (JobLogStreamStderr, "    no_differences: true")
    , (JobLogStreamStderr, "    # Red status if we encounter errors restyling")
    , (JobLogStreamStderr, "    error: true")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Request review on the Restyle PR?")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # Possible values:")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  #   author: From the author of the original PR")
    , (JobLogStreamStderr, "  #   owner: From the owner of the repository")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # One value will apply to origin and forked PRs, but you can also specify")
    , (JobLogStreamStderr, "  # separate values.")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  request_review: none")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Add labels to any created Restyle PRs")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # These can be used to tell other automation to avoid our PRs.")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  labels: []")
    , (JobLogStreamStderr, "  ")
    , (JobLogStreamStderr, "  # Which restylers to run")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # See restyled-io/restylers repository for their defaults.")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  restylers:")
    , (JobLogStreamStderr, "    - black:")
    , (JobLogStreamStderr, "        arguments:")
    , (JobLogStreamStderr, "          - \"--line-length 100\"")
    , (JobLogStreamStderr, "    - shfmt:")
    , (JobLogStreamStderr, "        arguments:")
    , (JobLogStreamStderr, "          - \"-i\"")
    , (JobLogStreamStderr, "          - \"2\"")
    , (JobLogStreamStderr, "          - \"-ci\"")
    , (JobLogStreamStderr, "          - \"-bn\"")
    , (JobLogStreamStderr, "          - \"-sr\"")
    , (JobLogStreamStderr, "    - prettier:")
    , (JobLogStreamStderr, "      include:")
    , (JobLogStreamStderr, "        - \"**/*.js\"")
    , (JobLogStreamStderr, "        - \"**/*.jsx\"")
    , (JobLogStreamStderr, "        - \"**/*.yml\"")
    , (JobLogStreamStderr, "        - \"**/*.yaml\"")
    , (JobLogStreamStderr, "        - \"!chats/**/*\"")
    , (JobLogStreamStderr, "        - \"!flow/**/*\"")
    , (JobLogStreamStderr, "   ")
    , (JobLogStreamStderr, "  # Version of the set of Restylers to run")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  # This should correspond to a ref on the restyled-io/restylers repository,")
    , (JobLogStreamStderr, "  # usually it's a tag that is a date of when that set was released. You could")
    , (JobLogStreamStderr, "  # re-specific the default in your own config if you prefer to avoid update")
    , (JobLogStreamStderr, "  # surprises.")
    , (JobLogStreamStderr, "  #")
    , (JobLogStreamStderr, "  restylers_version: \"20190715\"")
    , (JobLogStreamStderr, "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml")
    ]

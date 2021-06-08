module Restyled.Development.Seeds
    ( seedDB
    ) where

import Restyled.Prelude

import qualified Prelude as Unsafe
import Restyled.Marketplace
import Restyled.Models
import Restyled.PrivateRepoAllowance

seedDB :: MonadIO m => SqlPersistT m ()
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
    :: MonadIO m
    => Repo
    -> PullRequestNum
    -> UTCTime
    -> Maybe Int
    -> [(Text, Text)]
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
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

    -- Capture it "running"
    jobLogLines <- for timestamped $ \(t, (stream, content)) -> insertRecord
        JobLogLine
            { jobLogLineJob = jobId
            , jobLogLineCreatedAt = t
            , jobLogLineStream = stream
            , jobLogLineContent = content
            }

    -- Complete it if appropriate
    for_ mExitCode $ \ec -> do
        let lastLogLineAt = jobLogLineCreatedAt $ Unsafe.last jobLogLines
            completedAt = addUTCTime 1 lastLogLineAt

        insert_ JobLogLine
            { jobLogLineJob = jobId
            , jobLogLineCreatedAt = completedAt
            , jobLogLineStream = "system"
            , jobLogLineContent = "Restyler exited " <> tshow ec
            }

        update
            jobId
            [ JobUpdatedAt =. completedAt
            , JobCompletedAt =. Just completedAt
            , JobExitCode =. Just ec
            ]
  where
    timestamped =
        zip (secondsFrom createdAt)
            $ ("system", "docker run --rm ...")
            : ("system", "Running on ...")
            : untimestamped

secondsFrom :: UTCTime -> [UTCTime]
secondsFrom t0 = let t1 = addUTCTime 1 t0 in t1 : secondsFrom t1

-- brittany-next-binding --columns=250

restylingOutput :: [(Text, Text)]
restylingOutput =
    [ ("stderr", "Switched to a new branch 'issue#87'")
    , ("stdout", "Branch 'issue#87' set up to track remote branch 'issue#87' from 'origin'.")
    , ("stdout", "Restyling restyled-io/restyler#88")
    , ("stdout", "Restyled PR does not exist")
    , ("stderr", "Switched to a new branch 'issue#87-restyled'")
    , ("stdout", "Restyling \"app/Http/Controllers/Store.php\" via \"php-cs-fixer\"")
    , ("stderr", "Loaded config default from \"/code/.php_cs\".")
    , ("stderr", "Paths from configuration file have been overridden by paths provided as command arguments.")
    , ("stdout", "")
    , ("stdout", "Fixed all files in 0.010 seconds, 10.000 MB memory used")
    , ("stdout", "Restyling \"app/Models/Example.php\" via \"php-cs-fixer\"")
    ]

-- brittany-next-binding --columns=250

noDifferencesOutput :: [(Text, Text)]
noDifferencesOutput =
    [ ("stdout", "Branch 'lucky/ela-skills-debugger' set up to track remote branch 'lucky/ela-skills-debugger' from 'origin'.")
    , ("stderr", "Switched to a new branch 'lucky/ela-skills-debugger'")
    , ("stdout", "Restyling freckle/megarepo#8616")
    , ("stdout", "Restyled PR does not exist")
    , ("stderr", "Switched to a new branch 'lucky/ela-skills-debugger-restyled'")
    , ("stdout", "Setting status of no differences for 686fe0a")
    , ("stdout", "No style differences found")
    ]

-- brittany-next-binding --columns=250

invalidArgumentOutput :: [(Text, Text)]
invalidArgumentOutput =
    [ ("stderr", "From https://github.com/restyled.io/demo")
    , ("stderr", " * [new branch]      release/1 -> release/1")
    , ("stderr", "Switched to a new branch 'trim-fixes'")
    , ("stdout", "Branch 'trim-fixes' set up to track remote branch 'trim-fixes' from 'origin'.")
    , ("stdout", "Restyling restyled.io/demo#1")
    , ("stdout", "Restyled PR does not exist")
    , ("stderr", "Switched to a new branch 'elevator-trim-fixes-restyled'")
    , ("stderr", "Process unsuccessful (ExitFailure 127)")
    , ("stderr", "Command: docker")
    , ("stderr", "Arguments: [--rm, --net=none, ...]")
    , ("stderr", "")
    , ("stderr", "")
    , ("stderr", "  docker: command not found")
    , ("stderr", "    1:some/stack")
    , ("stderr", "    75:trace/there")
    , ("stderr", "")
    , ("stderr", "Please see https://google.com")
    , ("stderr", "")
    , ("stderr", "Please see")
    , ("stderr", "")
    , ("stderr", "  - https://google.com")
    , ("stderr", "  - https://google.com")
    , ("stderr", "  - https://google.com")
    ]

-- brittany-next-binding --columns=250

configErrorOutput1 :: [(Text, Text)]
configErrorOutput1 =
    [ ("stderr", "Switched to a new branch 'fix'")
    , ("stdout", "Branch 'fix' set up to track remote branch 'fix' from 'origin'.")
    , ("stderr", "Switched to a new branch 'fix-restyled'")
    , ("stderr", "We had trouble with your configuration:")
    , ("stderr", "")
    , ("stderr", "  Yaml parse exception:")
    , ("stderr", "  Aeson exception:")
    , ("stderr", "  Error in $.restylers[2]: - Unexpected key \"prettier\", must be one of")
    , ("stderr", "  [\"name\",\"image\",\"command\",\"arguments\",\"include\",\"interpreters\",\"supports_arg_sep\",\"supports_multiple_paths\",\"documentation\"].")
    , ("stderr", "  ")
    , ("stderr", "  ")
    , ("stderr", "  Did you intend to specify a full Restyler object, or do you have incorrect")
    , ("stderr", "  indentation for a named override?")
    , ("stderr", "  ")
    , ("stderr", "  Original input:")
    , ("stderr", "  # Restyler Configuration")
    , ("stderr", "  #")
    , ("stderr", "  # Overall notes:")
    , ("stderr", "  #")
    , ("stderr", "  # - All keys are optional and default as shown")
    , ("stderr", "  # - The entire config can also be just a list of values, which will be")
    , ("stderr", "  #   interpreted as specifying the restylers key")
    , ("stderr", "  #")
    , ("stderr", "  ####")
    , ("stderr", "  ")
    , ("stderr", "  # Do anything at all?")
    , ("stderr", "  enabled: true")
    , ("stderr", "  ")
    , ("stderr", "  # Push the style fixes directly to the original PR")
    , ("stderr", "  #")
    , ("stderr", "  # This setting implies pull_requests: false for origin PRs, and has no effect on")
    , ("stderr", "  # forked PRs (since we can't push to those).")
    , ("stderr", "  #")
    , ("stderr", "  auto: false")
    , ("stderr", "  ")
    , ("stderr", "  # Download remote files before restyling")
    , ("stderr", "  #")
    , ("stderr", "  # Example:")
    , ("stderr", "  #")
    , ("stderr", "  #   remote_files:")
    , ("stderr", "  #     - url: https://raw.github.com/.../hlint.yaml")
    , ("stderr", "  #       path: .hlint.yaml")
    , ("stderr", "  #")
    , ("stderr", "  # Files must be publicly accessible.")
    , ("stderr", "  #")
    , ("stderr", "  remote_files: []")
    , ("stderr", "  ")
    , ("stderr", "  # Open Restyle PRs?")
    , ("stderr", "  pull_requests: true")
    , ("stderr", "  ")
    , ("stderr", "  # Leave comments on the original PR linking to the Restyle PR?")
    , ("stderr", "  comments: true")
    , ("stderr", "  ")
    , ("stderr", "  # Set commit statuses on the original PR?")
    , ("stderr", "  statuses:")
    , ("stderr", "    # Red status in the case of differences found")
    , ("stderr", "    differences: true")
    , ("stderr", "    # Green status in the case of no differences found")
    , ("stderr", "    no_differences: true")
    , ("stderr", "    # Red status if we encounter errors restyling")
    , ("stderr", "    error: true")
    , ("stderr", "  ")
    , ("stderr", "  # Request review on the Restyle PR?")
    , ("stderr", "  #")
    , ("stderr", "  # Possible values:")
    , ("stderr", "  #")
    , ("stderr", "  #   author: From the author of the original PR")
    , ("stderr", "  #   owner: From the owner of the repository")
    , ("stderr", "  #")
    , ("stderr", "  # One value will apply to origin and forked PRs, but you can also specify")
    , ("stderr", "  # separate values.")
    , ("stderr", "  #")
    , ("stderr", "  request_review: none")
    , ("stderr", "  ")
    , ("stderr", "  # Add labels to any created Restyle PRs")
    , ("stderr", "  #")
    , ("stderr", "  # These can be used to tell other automation to avoid our PRs.")
    , ("stderr", "  #")
    , ("stderr", "  labels: []")
    , ("stderr", "  ")
    , ("stderr", "  # Which restylers to run")
    , ("stderr", "  #")
    , ("stderr", "  # See restyled-io/restylers repository for their defaults.")
    , ("stderr", "  #")
    , ("stderr", "  restylers:")
    , ("stderr", "    - black:")
    , ("stderr", "        arguments:")
    , ("stderr", "          - \"--line-length 100\"")
    , ("stderr", "    - shfmt:")
    , ("stderr", "        arguments:")
    , ("stderr", "          - \"-i\"")
    , ("stderr", "          - \"2\"")
    , ("stderr", "          - \"-ci\"")
    , ("stderr", "          - \"-bn\"")
    , ("stderr", "          - \"-sr\"")
    , ("stderr", "    - prettier:")
    , ("stderr", "      include:")
    , ("stderr", "        - \"**/*.js\"")
    , ("stderr", "        - \"**/*.jsx\"")
    , ("stderr", "        - \"**/*.yml\"")
    , ("stderr", "        - \"**/*.yaml\"")
    , ("stderr", "        - \"!chats/**/*\"")
    , ("stderr", "        - \"!flow/**/*\"")
    , ("stderr", "   ")
    , ("stderr", "  # Version of the set of Restylers to run")
    , ("stderr", "  #")
    , ("stderr", "  # This should correspond to a ref on the restyled-io/restylers repository,")
    , ("stderr", "  # usually it's a tag that is a date of when that set was released. You could")
    , ("stderr", "  # re-specific the default in your own config if you prefer to avoid update")
    , ("stderr", "  # surprises.")
    , ("stderr", "  #")
    , ("stderr", "  restylers_version: \"20190715\"")
    , ("stderr", "")
    , ("stderr", "Please see https://github.com/restyled-io/restyled.io/wiki/Common-Errors:-.restyled.yaml")
    , ("stderr", "")
    ]

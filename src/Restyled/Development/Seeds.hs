module Restyled.Development.Seeds
    ( seedDB
    )
where

import Restyled.Prelude

import qualified Prelude as Unsafe
import Restyled.Backend.Marketplace
import Restyled.Models

seedDB :: MonadIO m => SqlPersistT m ()
seedDB = do
    now <- liftIO getCurrentTime

    Entity _ demo <- upsertRepo (restyledRepo "demo")
    Entity _ restyled <- upsertRepo (restyledRepo "restyled.io")
    Entity _ restyler <- upsertRepo (restyledRepo "restyler")
    Entity _ _ops <- upsertRepo ((restyledRepo "ops") { repoIsPrivate = True })

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
            }
        []

    seedJob
        demo
        1
        now
        (Just 127)
        [ ("stderr", "From https://github.com/restyled.io/demo")
        , ("stderr", " * [new branch]      release/1 -> release/1")
        , ("stderr", "Switched to a new branch 'trim-fixes'")
        , ( "stdout"
          , "Branch 'trim-fixes' set up to track remote branch 'trim-fixes' from 'origin'."
          )
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

    seedJob
        restyled
        1
        now
        (Just 0)
        [ ( "stdout"
          , "Branch 'lucky/ela-skills-debugger' set up to track remote branch 'lucky/ela-skills-debugger' from 'origin'."
          )
        , ("stderr", "Switched to a new branch 'lucky/ela-skills-debugger'")
        , ("stdout", "Restyling freckle/megarepo#8616")
        , ("stdout", "Restyled PR does not exist")
        , ( "stderr"
          , "Switched to a new branch 'lucky/ela-skills-debugger-restyled'"
          )
        , ("stdout", "Setting status of no differences for 686fe0a")
        , ("stdout", "No style differences found")
        ]

    seedJob
        restyler
        1
        now
        Nothing
        [ ("stderr", "Switched to a new branch 'issue#87'")
        , ( "stdout"
          , "Branch 'issue#87' set up to track remote branch 'issue#87' from 'origin'."
          )
        , ("stdout", "Restyling restyled-io/restyler#88")
        , ("stdout", "Restyled PR does not exist")
        , ("stderr", "Switched to a new branch 'issue#87-restyled'")
        , ( "stdout"
          , "Restyling \"app/Http/Controllers/Store.php\" via \"php-cs-fixer\""
          )
        , ("stderr", "Loaded config default from \"/code/.php_cs\".")
        , ( "stderr"
          , "Paths from configuration file have been overridden by paths provided as command arguments."
          )
        , ("stdout", "")
        , ("stdout", "Fixed all files in 0.010 seconds, 10.000 MB memory used")
        , ( "stdout"
          , "Restyling \"app/Models/Example.php\" via \"php-cs-fixer\""
          )
        ]

    Entity discountPlanId _ <- fetchDiscountMarketplacePlan

    void $ upsert
        MarketplaceAccount
            { marketplaceAccountGithubId = 50812
            , marketplaceAccountGithubLogin = "pbrisbin"
            , marketplaceAccountMarketplacePlan = discountPlanId
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
    }

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

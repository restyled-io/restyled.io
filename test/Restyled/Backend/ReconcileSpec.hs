module Restyled.Backend.ReconcileSpec
    ( spec
    )
where

import Restyled.Test

import qualified Database.Persist as P
import Restyled.Backend.Container
import Restyled.Backend.DockerRunJob (chomp)
import Restyled.Backend.Reconcile

spec :: Spec
spec = withApp $ do
    describe "reconcileMachine" $ do
        -- Keep this test off of CI. Un-pend it if working in this area
        xit "finishes jobs for stopped containers" $ do
            jobId <- runDB createInFlightJob
            void $ createExitedContainer jobId

            result <- reconcileMachine

            result `shouldBe` ([], 1)
            cs <- getStoppedContainers
            cs `shouldBe` Right []
            Just Job {..} <- runDB $ P.get jobId
            jobCompletedAt `shouldSatisfy` isJust
            jobExitCode `shouldBe` Just 2

createInFlightJob :: MonadIO m => SqlPersistT m JobId
createInFlightJob = do
    now <- getCurrentTime
    insert Job
        { jobSvcs = GitHubSVCS
        , jobOwner = "owner"
        , jobRepo = "repo"
        , jobPullRequest = 1
        , jobCreatedAt = now
        , jobUpdatedAt = now
        , jobCompletedAt = Nothing
        , jobExitCode = Nothing
        , jobLog = Nothing
        , jobStdout = Nothing
        , jobStderr = Nothing
        }

-- NB. Test failures may leave containers lying around, clean them up with:
--
-- @
-- docker ps --all \
--   --filter status=exited \
--   --filter label=restyler-test \
--   --format {{.ID}} |
--   xargs docker rm
-- @
--
createExitedContainer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => JobId
    -> m String
createExitedContainer jobId = do
    let
        runArgs =
            [ "run"
            , "--detach"
            , "--label=restyler"
            , "--label=restyler-test"
            , "--label=job-id=" <> unpack (toPathPiece jobId)
            , "alpine"
            , "sh"
            , "-c"
            , "exit 2"
            ]
    containerId <- chomp <$> proc "docker" runArgs readProcessStdout_
    containerId <$ proc "docker" ["wait", containerId] readProcessStdout_

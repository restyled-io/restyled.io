module Restyled.Backend.DockerRunJob
    ( dockerRunJob

    -- * Useful utility
    -- | TODO: find a new home
    , chomp
    ) where

import Restyled.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Restyled.Backend.DockerRunArgs
import Restyled.Backend.MissingDaemonError
import Restyled.JobLogLine
import Restyled.Models
import Restyled.Settings

-- | @docker run@ on a @'RestyleMachine'@
--
-- Implementation details:
--
-- This should behave like @docker run --rm@ in that it runs a container, waits
-- for it, removes it, and returns its exit code. However, we do the steps
-- separately so that, if we die, the container will continue to run and
-- (presumably) complete the restyle. An unremoved container can be found later
-- and reconciled with the open Job state we'd leave in this case.
--
-- Any exceptions here will be logged and result in exit-code 99. The final
-- docker-rm is unchecked; it failing is ignored. Reconciliation should clean
-- this up, but may not find an open Job to reconcile it to.
--
dockerRunJob
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSqlPool env
       , HasSettings env
       , HasProcessContext env
       , HasAWS env
       )
    => Entity Repo
    -> Entity Job
    -> m ExitCode
dockerRunJob (Entity _ repo) job = do
    settings <- view settingsL
    token <- repoInstallationToken settings repo
    handleAny (logUnexpectedException job)
        $ handleJust toMissingDaemonError onMissingDaemonError
        $ dockerRunRm
        $ dockerRunArgs settings token repo job

dockerRunRm
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => [String]
    -> m ExitCode
dockerRunRm args = do
    container <-
        chomp <$> proc "docker" ("run" : "--detach" : args) readProcessStdout_
    exitCode <-
        readExitCode
        . chomp
        <$> proc "docker" ["wait", container] readProcessStdout_
    exitCode <$ proc "docker" ["rm", container] readProcess

logUnexpectedException
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSettings env
       , HasAWS env
       , Exception ex
       )
    => Entity Job
    -> ex
    -> m ExitCode
logUnexpectedException (Entity jobId _) ex = do
    logError $ "dockerRunJob: " <> fromString (displayException ex)
    captureJobLogLine
        jobId
        JobLogStreamSystem
        "An unexpected exception occurred when running this Job"
    pure $ ExitFailure 99

readExitCode :: String -> ExitCode
readExitCode s = case readMaybe s of
    Nothing -> ExitFailure 99
    Just 0 -> ExitSuccess
    Just i -> ExitFailure i

chomp :: LBS.ByteString -> String
chomp = LBS8.unpack . LBS8.reverse . LBS8.dropWhile isSpace . LBS8.reverse

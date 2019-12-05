{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.DockerRunJob
    ( dockerRunJob
    , followJobContainer
    )
where

import Restyled.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Time.ISO8601 (formatISO8601)
import Restyled.Backend.DockerRunArgs
import Restyled.Backend.RestyleMachine (withRestyleMachineEnv)
import Restyled.Models
import Restyled.Settings

-- | @docker run@ on a @'RestyleMachine'@
--
-- Implementation details:
--
-- This should behave like @docker run --rm@ in that it runs a container,
-- captures its output, removes it, and returns its exit code. However, we do
-- the steps separately so that, if we die, the container will continue to run
-- and (presumably) complete the restyle. An unremoved container can be found
-- later and reconciled with the open Job state we'd leave in this case.
--
-- Any exceptions here will be logged and result in exit-code 99. The final
-- docker-rm is unchecked; it failing is ignored. Reconciliation should clean
-- this up, but may not find an open Job to reconcile it to.
--
dockerRunJob
    :: (HasLogFunc env, HasDB env, HasSettings env, HasProcessContext env)
    => Entity Repo
    -> Entity Job
    -> Maybe (Entity RestyleMachine)
    -> RIO env ExitCode
    -- ^ Exit code will be @99@ if there was some exception in this process
dockerRunJob (Entity _ repo) job mMachine = do
    settings <- view settingsL
    token <- repoInstallationToken settings repo
    let args = "run" : "--detach" : dockerRunArgs settings token repo job

    handleAny loggedExitFailure $ withEnv $ do
        container <- chomp <$> proc "docker" args readProcessStdout_

        runDB $ update
            (entityKey job)
            [ JobMachineName =. Just machineName
            , JobContainerId =. Just (pack container)
            ]

        capture job "system"
            $ "Running on "
            <> unpack machineName
            <> " ("
            <> container
            <> ")"

        followJobContainer Nothing job container
  where
    (machineName, withEnv) = maybe
        ("localhost", id)
        ((restyleMachineName &&& withRestyleMachineEnv) . entityVal)
        mMachine

followJobContainer
    :: (HasLogFunc env, HasDB env, HasProcessContext env)
    => Maybe UTCTime
    -> Entity Job
    -> String -- ^ Container Id
    -> RIO env ExitCode
followJobContainer mTimestamp job container = do
    waitAsync <-
        async
        $ readExitCode
        . chomp
        <$> proc "docker" ["wait", container] readProcessStdout_

    -- If we're reconciling an old Job, capture the logs since we lost it
    let since = maybe [] (\t -> ["--since", formatISO8601 t]) mTimestamp

    logsAsync <-
        async
        $ proc "docker" (["logs", "--follow"] <> since <> [container])
        $ followProcess (capture job "stdout") (capture job "stderr")

    exitCode <- wait waitAsync <* wait logsAsync
    capture job "system" $ "Restyler exited " <> displayExitCode exitCode
    exitCode <$ proc "docker" ["rm", container] runProcess

capture
    :: (HasLogFunc env, HasDB env) => Entity Job -> Text -> String -> RIO env ()
capture job stream msg = do
    when (stream == "system") $ logDebug $ fromString msg
    runDB $ captureJobLogLine (entityKey job) stream $ pack msg

loggedExitFailure :: (HasLogFunc env, Show ex) => ex -> RIO env ExitCode
loggedExitFailure ex = ExitFailure 99 <$ logError msg
    where msg = "RestyleMachine.dockerRunJob: " <> displayShow ex

readExitCode :: String -> ExitCode
readExitCode s = case readMaybe s of
    Nothing -> ExitFailure 99
    Just 0 -> ExitSuccess
    Just i -> ExitFailure i

displayExitCode :: ExitCode -> String
displayExitCode = \case
    ExitSuccess -> "0"
    ExitFailure i -> show i

chomp :: LBS.ByteString -> String
chomp = LBS8.unpack . LBS8.reverse . LBS8.dropWhile isSpace . LBS8.reverse

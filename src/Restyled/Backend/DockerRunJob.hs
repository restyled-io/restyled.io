module Restyled.Backend.DockerRunJob
    ( followJobContainer

    -- * Useful utility
    -- | TODO: find a new home
    , chomp
    ) where

import Restyled.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Time.ISO8601 (formatISO8601)
import Restyled.Models

followJobContainer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSqlPool env
       , HasProcessContext env
       )
    => Maybe UTCTime
    -> Entity Job
    -> String -- ^ Container Id
    -> m ExitCode
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
    exitCode <$ proc "docker" ["rm", container] readProcess

capture
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasSqlPool env)
    => Entity Job
    -> Text
    -> String
    -> m ()
capture job stream msg = do
    when (stream == "system") $ logDebug $ fromString msg
    runDB $ captureJobLogLine (entityKey job) stream $ pack msg

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

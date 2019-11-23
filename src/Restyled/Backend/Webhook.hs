{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , processWebhook
    )
where

import Restyled.Prelude

import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Time (diff)
import Restyled.Backend.AcceptedJob
import Restyled.Backend.AcceptedWebhook
import Restyled.Backend.ExecRestyler
import Restyled.Models

enqueueWebhook :: ByteString -> Redis ()
enqueueWebhook = void . lpush queueName . pure

awaitWebhook
    :: (HasLogFunc env, HasRedis env, MonadReader env m, MonadIO m)
    => Integer
    -> m (Maybe ByteString)
awaitWebhook t = do
    logDebug "Awaiting webhook"
    eresult <- runRedis $ brpop [queueName] t
    logDebug $ "Popped: " <> displayShow eresult
    pure $ either (const Nothing) (snd <$>) eresult

data JobNotProcessed
    = WebhookIgnored IgnoredWebhookReason
    | JobIgnored (Entity Job) IgnoredJobReason
    | ExecRestylerFailure (Entity Job) SomeException

data JobProcessed = ExecRestylerSuccess (Entity Job) ExitCode

processWebhook
    :: (HasLogFunc env, HasDB env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body = exceptT fromNotProcessed fromProcessed $ do
    webhook <- withExceptT WebhookIgnored $ acceptWebhook body
    job <- withExceptT (JobIgnored $ awJob webhook) $ acceptJob webhook

    let failure = ExecRestylerFailure $ ajJob job
        success = ExecRestylerSuccess $ ajJob job

    logInfo $ fromString $ "Executing Restyler for " <> jobPath (ajJob job)
    withExceptT failure $ success <$> tryExecRestyler execRestyler job

fromNotProcessed :: (HasLogFunc env, HasDB env) => JobNotProcessed -> RIO env ()
fromNotProcessed = \case
    WebhookIgnored reason ->
        logDebug $ fromString $ "Webhook ignored: " <> reasonToLogMessage reason
    JobIgnored job reason -> do
        logWarn
            $ fromString
            $ "Job ignored for "
            <> jobPath job
            <> ": "
            <> ignoredJobReasonToLogMessage reason
        runDB $ completeJobSkipped (ignoredJobReasonToJobLogLine reason) job
    ExecRestylerFailure job ex -> do
        logError
            $ fromString
            $ "Exec failure for "
            <> jobPath job
            <> ": "
            <> show ex
        runDB $ completeJobErrored (show ex) job

fromProcessed :: (HasLogFunc env, HasDB env) => JobProcessed -> RIO env ()
fromProcessed (ExecRestylerSuccess job ec) = do
    runDB $ completeJob ec job

    -- Don't use Job attributes to log the Outcome, since that would require
    -- reloading from DB. Might as well avoid it since we have all the
    -- completion values here anyway.
    now <- getCurrentTime
    logInfo
        $ fromString
        $ "Job completed for "
        <> jobPath job
        <> " ("
        <> jobOutcome ec (jobCreatedAt $ entityVal job) now
        <> ")"

jobPath :: Entity Job -> String
jobPath (Entity _ Job {..}) =
    unpack $ repoPullPath jobOwner jobRepo jobPullRequest

jobOutcome :: ExitCode -> UTCTime -> UTCTime -> String
jobOutcome ec createdAt completedAt = "exited " <> status <> " in " <> duration
  where
    status = case ec of
        ExitSuccess -> "0"
        ExitFailure i -> show i

    duration =
        TL.unpack $ format (diff False) $ diffUTCTime createdAt completedAt

queueName :: ByteString
queueName = "restyled:hooks:webhooks"

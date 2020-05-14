{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , processWebhook
    , queueDepth
    )
where

import Restyled.Prelude

import Restyled.Backend.AcceptedJob
import Restyled.Backend.AcceptedWebhook
import Restyled.Backend.ConcurrentJobs
import Restyled.Backend.ExecRestyler
import Restyled.Backend.RestyleMachine
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
    :: (HasLogFunc env, HasProcessContext env, HasDB env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body = withRestyleMachine $ \mMachine ->
    exceptT fromNotProcessed fromProcessed $ do
        webhook <- withExceptT WebhookIgnored $ acceptWebhook body
        job <- withExceptT (JobIgnored $ awJob webhook) $ acceptJob webhook

        let acceptedJob = ajJob job
            failure = ExecRestylerFailure acceptedJob
            success = ExecRestylerSuccess acceptedJob

        -- N.B. Ideally, we'd only cancel stale Jobs once the new Job is
        -- started, but since we operate under single-threaded waiting we can't
        -- do that. Some day we'll fire-forget-and-track, unblocking that
        -- pattern.
        lift $ cancelStaleJobs $ ajStaleJobs job

        logDebug $ "Executing Restyler for " <> display (jobPath acceptedJob)
        withExceptT failure
            $ success
            <$> tryExecRestyler execRestyler job mMachine

fromNotProcessed :: (HasLogFunc env, HasDB env) => JobNotProcessed -> RIO env ()
fromNotProcessed = \case
    WebhookIgnored reason ->
        logDebug $ fromString $ "Webhook ignored: " <> reasonToLogMessage reason
    JobIgnored job reason -> do
        logWarn
            $ "Job ignored for "
            <> display (jobPath job)
            <> ": "
            <> fromString (ignoredJobReasonToLogMessage reason)
        void $ runDB $ completeJobSkipped
            (ignoredJobReasonToJobLogLine reason)
            job
    ExecRestylerFailure job ex -> do
        logError
            $ "Exec failure for "
            <> display (jobPath job)
            <> ": "
            <> displayShow ex
        void $ runDB $ completeJobErrored (show ex) job

fromProcessed :: (HasLogFunc env, HasDB env) => JobProcessed -> RIO env ()
fromProcessed (ExecRestylerSuccess job ec) = do
    updatedJob <- runDB $ completeJob ec job
    logInfo
        $ "Job completed for "
        <> display (jobPath job)
        <> " ("
        <> display (jobOutcome $ entityVal updatedJob)
        <> ")"

queueDepth :: Redis (Maybe Integer)
queueDepth = hush <$> llen queueName

queueName :: ByteString
queueName = "restyled:hooks:webhooks"

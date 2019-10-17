{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , processWebhook
    )
where

import Restyled.Prelude

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
processWebhook execRestyler =
    exceptT fromNotProcessed fromProcessed
        . processWebhookFromT execRestyler
        . acceptWebhook

processWebhookFromT
    :: ExecRestyler (RIO env)
    -> ExceptT IgnoredWebhookReason (RIO env) AcceptedWebhook
    -> ExceptT JobNotProcessed (RIO env) JobProcessed
processWebhookFromT execRestyler getWebhook =
    withExceptT WebhookIgnored getWebhook
        >>= acceptWebhookJob
        >>= restyleAcceptedJob execRestyler

acceptWebhookJob
    :: AcceptedWebhook -> ExceptT JobNotProcessed (RIO env) AcceptedJob
acceptWebhookJob webhook@AcceptedWebhook {..} =
    withExceptT (JobIgnored awJob) $ acceptJob webhook

restyleAcceptedJob
    :: ExecRestyler (RIO env)
    -> AcceptedJob
    -> ExceptT JobNotProcessed (RIO env) JobProcessed
restyleAcceptedJob execRestyler job@AcceptedJob {..} =
    withExceptT (ExecRestylerFailure ajJob)
        $ ExecRestylerSuccess ajJob
        <$> tryExecRestyler execRestyler job

fromNotProcessed :: (HasLogFunc env, HasDB env) => JobNotProcessed -> RIO env ()
fromNotProcessed = \case
    WebhookIgnored reason ->
        logWarn $ fromString $ "Webhook ignored: " <> reasonToLogMessage reason
    JobIgnored job reason -> do
        logWarn
            $ fromString
            $ "Job ignored: "
            <> ignoredJobReasonToLogMessage reason
        runDB $ completeJobSkipped (ignoredJobReasonToJobLogLine reason) job
    ExecRestylerFailure job ex -> do
        logError $ "Exec failure: " <> displayShow ex
        runDB $ completeJobErrored (show ex) job

fromProcessed :: HasDB env => JobProcessed -> RIO env ()
fromProcessed (ExecRestylerSuccess job ec) = runDB $ completeJob ec job

queueName :: ByteString
queueName = "restyled:hooks:webhooks"

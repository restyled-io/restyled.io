{-# LANGUAGE LambdaCase #-}

module Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , webhookQueueName

    -- * Processing
    , processWebhook
    , processWebhookExceptT
    )
where

import Import hiding (runDB, timeout)

import Backend.AcceptedJob
import Backend.AcceptedWebhook
import Backend.DB
import Backend.ExecRestyler
import Backend.Foundation
import Control.Monad.Except

enqueueWebhook :: MonadBackend m => ByteString -> m ()
enqueueWebhook body = do
    logDebugN "Enqueuing Webhook..."
    void $ runRedis $ lpush webhookQueueName [body]

awaitWebhook :: MonadBackend m => Integer -> m (Maybe ByteString)
awaitWebhook timeout = do
    logDebugN "Awaiting Webhook..."
    eresult <- runRedis $ brpop [webhookQueueName] timeout
    logDebugN $ "Popped value: " <> tshow eresult
    pure $ either (const Nothing) (snd <$>) eresult

webhookQueueName :: ByteString
webhookQueueName = "restyled:hooks:webhooks"

data JobNotProcessed
    = WebhookIgnored IgnoredWebhookReason
    | JobIgnored IgnoredJob
    | ExecRestylerFailure FailedExecRestyler

processWebhook :: MonadBackend m => ExecRestyler m -> ByteString -> m ()
processWebhook execRestyler body = do
    result <- runExceptT $ processWebhookExceptT execRestyler body

    mUpdatedJob <- case result of
        Left (WebhookIgnored reason) -> do
            logDebugN $ "Webhook ignored: " <> reasonToLogMessage reason
            pure Nothing
        Left (JobIgnored (IgnoredJob job)) -> do
            logWarnN "Job ignored"
            logDebugN $ jOut job
            pure $ Just job
        Left (ExecRestylerFailure (FailedExecRestyler job)) -> do
            logErrorN $ "ExecRestyler failure: " <> jErr job
            pure $ Just job
        Right (SucceededExecRestyler job) -> do
            logInfoN "ExecRestyler succeed"
            pure $ Just job

    for_ mUpdatedJob $ runDB . replaceEntity
  where
    jOut = fromMaybe "" . jobStdout . entityVal
    jErr = fromMaybe "" . jobStderr . entityVal
    replaceEntity (Entity k v) = replace k v

-- brittany-disable-next-binding

processWebhookExceptT
    :: MonadBackend m
    => ExecRestyler m
    -> ByteString
    -> ExceptT JobNotProcessed m SucceededExecRestyler
processWebhookExceptT execRestyler =
    withExceptT WebhookIgnored . acceptWebhook
        >=> withExceptT JobIgnored . acceptJob
        >=> withExceptT ExecRestylerFailure . runExecRestyler execRestyler

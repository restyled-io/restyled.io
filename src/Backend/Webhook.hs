module Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , webhookQueueName

    -- * Processing
    , processWebhook
    , processWebhookExceptT
    )
where

import Backend.Import

import Backend.AcceptedJob
import Backend.AcceptedWebhook
import Backend.ExecRestyler

enqueueWebhook :: (HasLogFunc env, HasRedis env) => ByteString -> RIO env ()
enqueueWebhook body = do
    logDebug "Enqueuing Webhook..."
    void $ runRedis $ lpush webhookQueueName [body]

awaitWebhook
    :: (HasLogFunc env, HasRedis env) => Integer -> RIO env (Maybe ByteString)
awaitWebhook timeout = do
    logDebug "Awaiting Webhook..."
    eresult <- runRedis $ brpop [webhookQueueName] timeout
    logDebug $ "Popped value: " <> displayShow eresult
    pure $ either (const Nothing) (snd <$>) eresult

webhookQueueName :: ByteString
webhookQueueName = "restyled:hooks:webhooks"

data JobNotProcessed
    = WebhookIgnored IgnoredWebhookReason
    | JobIgnored IgnoredJob
    | ExecRestylerFailure FailedExecRestyler

processWebhook
    :: (HasLogFunc env, HasDB env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body = do
    result <- runExceptT $ processWebhookExceptT execRestyler body

    mUpdatedJob <- case result of
        Left (WebhookIgnored reason) -> do
            logDebug $ "Webhook ignored: " <> fromString
                (unpack $ reasonToLogMessage reason)
            pure Nothing
        Left (JobIgnored (IgnoredJob job)) -> do
            logWarn "Job ignored"
            logDebug $ jOut job
            pure $ Just job
        Left (ExecRestylerFailure (FailedExecRestyler job)) -> do
            logError $ "ExecRestyler failure: " <> jErr job
            pure $ Just job
        Right (SucceededExecRestyler job) -> do
            logInfo "ExecRestyler succeed"
            pure $ Just job

    for_ mUpdatedJob $ runDB . replaceEntity
  where
    jOut = fromString . unpack . fromMaybe "" . jobStdout . entityVal
    jErr = fromString . unpack . fromMaybe "" . jobStderr . entityVal
    replaceEntity (Entity k v) = replace k v

-- brittany-disable-next-binding

processWebhookExceptT
    :: HasDB env
    => ExecRestyler (RIO env)
    -> ByteString
    -> ExceptT JobNotProcessed (RIO env) SucceededExecRestyler
processWebhookExceptT execRestyler =
    withExceptT WebhookIgnored . acceptWebhook
        >=> withExceptT JobIgnored . acceptJob
        >=> withExceptT ExecRestylerFailure . runExecRestyler execRestyler

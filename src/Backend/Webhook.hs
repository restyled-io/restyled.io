{-# LANGUAGE LambdaCase #-}

module Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , webhookQueueName

    -- * Processing
    , processWebhook
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
    | ExecRestylerFailure ExecRestylerFailed

processWebhook
    :: (HasLogFunc env, HasDB env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body = do
    mUpdatedJob <- exceptT fromFailure (pure . pure) $ do
        webhook <- withExceptT WebhookIgnored $ acceptWebhook body
        job <- withExceptT JobIgnored $ acceptJob webhook
        withExceptT ExecRestylerFailure $ runExecRestyler execRestyler job

    for_ mUpdatedJob $ runDB . replaceEntity

-- brittany-disable-next-binding

fromFailure :: HasLogFunc env => JobNotProcessed -> RIO env (Maybe (Entity Job))
fromFailure = \case
    WebhookIgnored reason -> logReturn Nothing
        $ "Webhook ignored: " <> reasonToLogMessage reason
    JobIgnored (IgnoredJob reason job) -> logReturn (Just job)
        $ "Job ignored: " <> ignoredJobReasonToLogMessage reason
    ExecRestylerFailure (ExecRestylerFailed ex job) -> logReturn (Just job)
        $ "Exec failure: " <> show ex
  where
    logReturn :: HasLogFunc env => Maybe a -> String -> RIO env (Maybe a)
    logReturn x msg = x <$ logWarn (fromString msg)

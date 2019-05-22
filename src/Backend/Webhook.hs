{-# LANGUAGE LambdaCase #-}

module Backend.Webhook
    ( webhookQueueName
    , enqueueWebhook
    , awaitWebhook
    , processWebhook
    )
where

import Backend.Import

import Backend.AcceptedJob
import Backend.AcceptedWebhook
import Backend.ExecRestyler

webhookQueueName :: ByteString
webhookQueueName = "restyled:hooks:webhooks"

enqueueWebhook :: HasRedis env => ByteString -> RIO env ()
enqueueWebhook = void . runRedis . lpush webhookQueueName . pure

awaitWebhook :: HasRedis env => Integer -> RIO env (Maybe ByteString)
awaitWebhook t = do
    eresult <- runRedis $ brpop [webhookQueueName] t
    pure $ either (const Nothing) (snd <$>) eresult

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

logReturn :: HasLogFunc env => Maybe a -> String -> RIO env (Maybe a)
logReturn x msg = x <$ logWarn (fromString msg)

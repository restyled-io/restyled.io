{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , processWebhook
    , queueDepth
    )
where

import Restyled.Prelude

import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Time (diff)
import Restyled.Backend.AcceptedJob
import Restyled.Backend.AcceptedWebhook
import Restyled.Backend.ExecRestyler
import Restyled.Backend.RestyleMachine
import Restyled.Models
import Restyled.Settings

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
    :: (HasSettings env, HasLogFunc env, HasProcessContext env, HasDB env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body =
    restyleMachineEnv $ exceptT fromNotProcessed fromProcessed $ do
        webhook <- withExceptT WebhookIgnored $ acceptWebhook body
        job <- withExceptT (JobIgnored $ awJob webhook) $ acceptJob webhook

        let failure = ExecRestylerFailure $ ajJob job
            success = ExecRestylerSuccess $ ajJob job

        logDebug $ fromString $ "Executing Restyler for " <> jobPath (ajJob job)
        withExceptT failure $ success <$> tryExecRestyler execRestyler job

restyleMachineEnv
    :: (HasSettings env, HasLogFunc env, HasProcessContext env, HasDB env)
    => RIO env a
    -> RIO env a
restyleMachineEnv act = do
    restyleMachineLocal <- appRestyleMachineLocal <$> view settingsL

    if restyleMachineLocal
        then act
        else withRestyleMachine $ (`withRestyleMachineEnv` act) . entityVal

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
        void $ runDB $ completeJobSkipped
            (ignoredJobReasonToJobLogLine reason)
            job
    ExecRestylerFailure job ex -> do
        logError
            $ fromString
            $ "Exec failure for "
            <> jobPath job
            <> ": "
            <> show ex
        void $ runDB $ completeJobErrored (show ex) job

fromProcessed :: (HasLogFunc env, HasDB env) => JobProcessed -> RIO env ()
fromProcessed (ExecRestylerSuccess job ec) = do
    updatedJob <- runDB $ completeJob ec job
    logInfo
        $ fromString
        $ "Job completed for "
        <> jobPath job
        <> " ("
        <> jobOutcome (entityVal updatedJob)
        <> ")"

jobPath :: Entity Job -> String
jobPath (Entity _ Job {..}) =
    unpack $ repoPullPath jobOwner jobRepo jobPullRequest

jobOutcome :: Job -> String
jobOutcome Job {..} = fromMaybe "N/A" $ do
    exitCode <- jobExitCode
    duration <- getDuration <$> jobCompletedAt
    pure $ "exited " <> show exitCode <> " in " <> duration
  where
    getDuration = TL.unpack . format (diff False) . diffUTCTime jobCreatedAt

queueDepth :: Redis (Maybe Integer)
queueDepth = hush <$> llen queueName

queueName :: ByteString
queueName = "restyled:hooks:webhooks"

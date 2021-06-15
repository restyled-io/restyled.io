module Restyled.Backend.Webhook
    ( enqueueWebhook
    , awaitWebhook
    , processWebhook
    , queueDepth
    ) where

import Restyled.Prelude

import Restyled.Backend.AcceptedJob
import Restyled.Backend.AcceptedWebhook
import Restyled.Backend.ConcurrentJobs
import Restyled.Backend.ExecRestyler
import Restyled.Backend.Reconcile
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
    :: (HasSettings env, HasLogFunc env, HasProcessContext env, HasSqlPool env)
    => ExecRestyler (RIO env)
    -> ByteString
    -> RIO env ()
processWebhook execRestyler body =
    restyleMachineEnv $ exceptT fromNotProcessed fromProcessed $ do
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

        logDebug $ display acceptedJob <> ": executing Restyler"
        eitherT failure success $ tryExecRestyler execRestyler job

restyleMachineEnv
    :: (HasSettings env, HasLogFunc env, HasProcessContext env, HasSqlPool env)
    => RIO env a
    -> RIO env a
restyleMachineEnv act = do
    restyleMachineLocal <- appRestyleMachineLocal <$> view settingsL

    if restyleMachineLocal
        then act
        else withRestyleMachine $ (`withRestyleMachineEnv` act) . entityVal

-- | Fetch a Machine, and increment its @jobCount@ during execution
--
-- Won't ever select an overloaded machine (appRestyleMachineJobsMax), and
-- instead blocks until one becomes available.
--
-- Risk: if we get into an issue where @restyle_machines.job_count@ is not being
-- managed correctly (and so never decrementing), we might have a no machines
-- available even if they're all empty.
--
withRestyleMachine
    :: (HasSettings env, HasSqlPool env, HasLogFunc env, HasProcessContext env)
    => (Entity RestyleMachine -> RIO env a)
    -> RIO env a
withRestyleMachine f = do
    jobsMax <- appRestyleMachineJobsMax <$> view settingsL
    machine <- throttleWarn $ runDB $ do
        mMachine <- selectFirst
            [ RestyleMachineEnabled ==. True
            , RestyleMachineJobCount <. fromIntegral jobsMax
            ]
            [Asc RestyleMachineJobCount]
        mMachine <$ traverse_ increment mMachine
    f machine `finally` runDB (decrement machine)
  where
    increment :: MonadIO m => Entity RestyleMachine -> SqlPersistT m ()
    increment = flip update [RestyleMachineJobCount +=. 1] . entityKey

    decrement :: MonadIO m => Entity RestyleMachine -> SqlPersistT m ()
    decrement = flip update [RestyleMachineJobCount -=. 1] . entityKey

throttleWarn
    :: (HasSqlPool env, HasLogFunc env, HasProcessContext env)
    => RIO env (Maybe a)
    -> RIO env a
throttleWarn act = do
    mVal <- act
    case mVal of
        Nothing -> do
            logWarn "No Restyle Machine available, sleeping 1m"

            -- Pause for some time, but also run a reconcile while we wait
            a <- async $ threadDelay $ delaySeconds * 1000000
            safelyReconcile delaySeconds Nothing
            wait a

            throttleWarn act
        Just val -> pure val
  where
    delaySeconds :: Int
    delaySeconds = 60

fromNotProcessed
    :: (HasLogFunc env, HasSqlPool env) => JobNotProcessed -> RIO env ()
fromNotProcessed = \case
    WebhookIgnored reason ->
        logDebug $ fromString $ "Webhook ignored: " <> reasonToLogMessage reason
    JobIgnored job reason -> do
        logWarn
            $ display job
            <> " ignored ("
            <> fromString (ignoredJobReasonToLogMessage reason)
            <> ")"
        void $ runDB $ completeJobSkipped
            (ignoredJobReasonToJobLogLine reason)
            job
    ExecRestylerFailure job ex -> do
        logError $ display job <> " exec failure (" <> displayShow ex <> ")"
        void $ runDB $ completeJobErrored (show ex) job

fromProcessed :: (HasLogFunc env, HasSqlPool env) => JobProcessed -> RIO env ()
fromProcessed (ExecRestylerSuccess job ec) = do
    updatedJob <- runDB $ completeJob ec job
    logInfo
        $ display updatedJob
        <> " completed ("
        <> display (jobOutcome $ entityVal updatedJob)
        <> ")"

queueDepth :: Redis (Maybe Integer)
queueDepth = hush <$> llen queueName

queueName :: ByteString
queueName = "restyled:hooks:webhooks"

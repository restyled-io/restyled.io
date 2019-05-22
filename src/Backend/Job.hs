module Backend.Job
    ( awaitRestylerJob
    , enqueueRestylerJob
    , queueName

    -- * Processing
    , processJob
    )
where

import Backend.Import

import Backend.AcceptedJob
import Backend.ExecRestyler

awaitRestylerJob
    :: (HasLogFunc env, HasRedis env) => Integer -> RIO env (Maybe (Entity Job))
awaitRestylerJob t = do
    logDebug "Awaiting Restyler Job..."
    eresult <- runRedis $ brpop [queueName] t
    logDebug $ "Popped value: " <> displayShow eresult
    return $ either (const Nothing) (decodePopped =<<) eresult
    where decodePopped = decodeStrict . snd

enqueueRestylerJob :: (HasLogFunc env, HasRedis env) => Entity Job -> RIO env ()
enqueueRestylerJob e@(Entity jid job) = do
    logDebug
        $ fromString
        $ "Enqueuing Restyler Job Id "
        <> unpack (toPathPiece jid)
        <> ": "
        <> show job
    void $ runRedis $ lpush queueName [encodeStrict e]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

processJob :: HasDB env => ExecRestyler (RIO env) -> Entity Job -> RIO env ()
processJob execRestyler eJob@(Entity jobId job) = do
    result <- runExceptT $ do
        repo <- noteT "Repo not found" $ MaybeT $ runDB $ fetchRepoForJob job
        withExceptT show $ tryExecRestyler execRestyler $ AcceptedJob
            { ajRepo = repo
            , ajJob = eJob
            }

    now <- liftIO getCurrentTime
    let failure = completeJobErrored now
        success = completeJob now

    runDB $ replace jobId $ either failure success result job

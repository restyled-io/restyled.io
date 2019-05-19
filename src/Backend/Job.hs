module Backend.Job
    ( awaitRestylerJob
    , enqueueRestylerJob
    , queueName

    -- * Processing
    , processJob
    ) where

import Backend.Import

import Backend.AcceptedJob
import Backend.ExecRestyler

awaitRestylerJob
    :: (HasLogFunc env, HasRedis env) => Integer -> RIO env (Maybe (Entity Job))
awaitRestylerJob timeout = do
    logDebug "Awaiting Restyler Job..."
    eresult <- runRedis $ brpop [queueName] timeout
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
    now <- liftIO getCurrentTime

    result <- runExceptT $ do
        repo <- noteT "Repo not found" $ MaybeT $ runDB $ fetchRepoForJob job
        withExceptT show $ tryExecRestyler execRestyler $ AcceptedJob
            { ajRepo = repo
            , ajJob = eJob
            }

    runDB $ replace jobId $ either
        (completeJobErroredMsg now)
        (completeJob now)
        result
        job

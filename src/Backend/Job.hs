module Backend.Job
    ( awaitRestylerJob
    , enqueueRestylerJob
    , queueName
    ) where

import Backend.Import

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

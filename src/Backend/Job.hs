module Backend.Job
    ( awaitRestylerJob
    , enqueueRestylerJob
    , queueName
    ) where

import Backend.Import

import Backend.Foundation

awaitRestylerJob :: MonadBackend m => Integer -> m (Maybe (Entity Job))
awaitRestylerJob timeout = do
    logDebugN "Awaiting Restyler Job..."
    eresult <- runRedis $ brpop [queueName] timeout
    logDebugN $ "Popped value: " <> tshow eresult
    return $ either (const Nothing) (decodePopped =<<) eresult
    where decodePopped = decodeStrict . snd

enqueueRestylerJob :: MonadBackend m => Entity Job -> m ()
enqueueRestylerJob e@(Entity jid job) = do
    logDebugN
        $ "Enqueuing Restyler Job Id "
        <> toPathPiece jid
        <> ": "
        <> tshow job
    void $ runRedis $ lpush queueName [encodeStrict e]

queueName :: ByteString
queueName = "restyled:restyler:jobs"

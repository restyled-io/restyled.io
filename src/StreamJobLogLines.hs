module StreamJobLogLines
    ( streamJobLogLines
    )
where

import Import

import Control.Concurrent (threadDelay)
import Network.WebSockets (Connection, ConnectionException, WebSocketsData)
import qualified Network.WebSockets as WS
import Widgets.JobLogLine
import Yesod.WebSockets hiding (sendClose)

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = handle ignoreConnectionException $ go 0
  where
    go offset = do
        -- Pre-emptively make sure someone is listening. If we don't do this,
        -- the case of empty log-lines will recurse indefinitely even if the
        -- client has closed their tab
        void $ sendTextDataAck @_ @Text ""

        -- Super naive throttle
        liftIO $ threadDelay $ 1 * 1000000
        logLines <- lift $ runDB $ fetchJobLogLines jobId offset

        for_ logLines $ \logLine -> do
            logDebugN "Sending JobLogLine"
            html <- lift $ renderJobLogLine logLine
            void $ sendTextDataAck html

        isInProgress <- lift $ runDB $ fetchJobIsInProgress jobId

        -- Always recurse if in progress or we're still streaming lines
        if isInProgress || not (null logLines)
            then go $ offset + length logLines
            else do
                logDebugN "Closing websocket: Job not in progress"
                sendClose @_ @Text "Job finished"

ignoreConnectionException :: Applicative f => ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck :: (MonadIO m, WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a *> receiveData

-- | <https://github.com/yesodweb/yesod/issues/1599>
sendClose
    :: (MonadIO m, WebSocketsData a, MonadReader Connection m) => a -> m ()
sendClose x = do
    conn <- ask
    liftIO $ WS.sendClose conn x

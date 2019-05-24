module Restyled.StreamJobLogLines
    ( streamJobLogLines
    )
where

import Restyled.Prelude

import qualified Network.WebSockets as WS
import Restyled.Foundation
import Restyled.Models
import Restyled.Widgets.JobLogLine
import Yesod.WebSockets hiding (sendClose)

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = handle ignoreConnectionException $ go 0
  where
    go offset = do
        -- Pre-emptively make sure someone is listening. If we don't do this,
        -- the case of empty log-lines will recurse indefinitely even if the
        -- client has closed their tab
        void $ sendTextDataAck @_ @Text ""

        (isInProgress, logLines) <-
            lift
            $ runDB
            $ (,)
            <$> fetchJobIsInProgress jobId
            <*> fetchJobLogLines jobId offset

        htmls <- traverse (lift . renderJobLogLine) logLines
        void $ sendTextDataAck $ mconcat htmls

        -- Always recurse if in progress or we're still streaming lines
        if isInProgress || not (null logLines)
            then go $ offset + length logLines
            else do
                logDebugN "Closing websocket: Job not in progress"
                sendClose @_ @Text "Job finished"

ignoreConnectionException :: Applicative f => WS.ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a *> receiveData

-- | <https://github.com/yesodweb/yesod/issues/1599>
sendClose
    :: (MonadIO m, WS.WebSocketsData a, MonadReader WS.Connection m)
    => a
    -> m ()
sendClose x = do
    conn <- ask
    liftIO $ WS.sendClose conn x

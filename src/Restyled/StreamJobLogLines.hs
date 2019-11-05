module Restyled.StreamJobLogLines
    ( streamJobLogLines
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.WebSockets
import Restyled.Widgets.JobLogLine

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = ignoringConnectionException $ go 0
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

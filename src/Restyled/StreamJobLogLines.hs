module Restyled.StreamJobLogLines
    ( streamJobLogLines
    ) where

import Restyled.Prelude

import Data.List.NonEmpty as NE
import Restyled.Foundation
import Restyled.Models
import Restyled.WebSockets
import Restyled.Widgets.JobLogLine

streamJobLogLines :: Entity Job -> WebSocketsT Handler ()
streamJobLogLines job = ignoringConnectionException $ go 0 Nothing
  where
    go lastBackoff mSince = do
        -- Pre-emptively make sure someone is listening. If we don't do this,
        -- the case of empty log-lines will recurse indefinitely even if the
        -- client has closed their tab
        void $ sendTextDataAck @_ @Text ""

        (isInProgress, logLines) <- do
            output <- lift $ runDB $ fetchJobOutput job mSince

            case output of
                JobOutputInProgress _ logLines -> pure (True, logLines)
                JobOutputCompleted logLines -> pure (False, logLines)
                JobOutputCompressed{} -> pure (False, []) -- Legacy

        htmls <- traverse (lift . renderJobLogLine) logLines
        void $ sendTextDataAck $ mconcat htmls

        -- If no lines, backoff more, up to a maximum of 5s; otherwise, reset
        let backoff = if null logLines then min 5 (lastBackoff + 1) else 0

        -- Always recurse if in progress or we're still streaming lines
        if isInProgress || not (null logLines)
            then do
                threadDelay $ backoff * 1000000
                go backoff $ getLastLineCreatedAt logLines <|> mSince
            else do
                logDebugN "Closing websocket: Job not in progress"
                sendClose @_ @Text "Job finished"

getLastLineCreatedAt :: [JobLogLine] -> Maybe UTCTime
getLastLineCreatedAt = fmap (jobLogLineCreatedAt . NE.last) . NE.nonEmpty

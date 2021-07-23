module Restyled.StreamJobLogLines
    ( streamJobLogLines
    ) where

import Restyled.Prelude

import qualified Data.List.NonEmpty as NE
import Restyled.Foundation
import Restyled.JobLogLine
import Restyled.Models
import Restyled.WebSockets
import Restyled.Widgets.JobLogLine

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = ignoringConnectionException $ go Nothing 0
  where
    go mSince lastBackoff = do
        -- Pre-emptively make sure someone is listening. If we don't do this,
        -- the case of empty log-lines will recurse indefinitely even if the
        -- client has closed their tab
        void $ sendTextDataAck @_ @Text ""

        isInProgress <- lift $ runDB $ fetchJobIsInProgress jobId
        jobLogLines <- lift $ fetchJobLogLinesSince mSince jobId

        -- If we got no output, increase the last backoff by 1 (5 max). Reset it
        -- whenever we do get output.
        let backoff = if null jobLogLines then min 5 (lastBackoff + 1) else 0
            mLastCreatedAt =
                jobLogLineCreatedAt . NE.last <$> NE.nonEmpty jobLogLines
            mNextSince = mLastCreatedAt <|> mSince

        logDebugN
            $ "Fetched Job Log"
            <> "\n  in-progress="
            <> tshow isInProgress
            <> "\n  log-lines="
            <> tshow (map jobLogLineCreatedAt jobLogLines)
            <> "\n  since="
            <> tshow mSince
            <> "\n  last-created="
            <> tshow mLastCreatedAt
            <> "\n  next-since="
            <> tshow mNextSince
            <> "\n  backoff="
            <> tshow backoff

        htmls <- traverse (lift . renderJobLogLine) jobLogLines
        void $ sendTextDataAck $ mconcat htmls

        -- Always recurse if in progress or we're still streaming lines
        if isInProgress || not (null jobLogLines)
            then do
                threadDelay $ backoff * 1000000
                go mNextSince backoff
            else do
                logDebugN "Closing websocket: Job not in progress"
                sendClose @_ @Text "Job finished"

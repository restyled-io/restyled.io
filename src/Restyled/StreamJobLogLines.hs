module Restyled.StreamJobLogLines
    ( streamJobLogLines
    ) where

import Restyled.Prelude

import Conduit
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import Restyled.Foundation
import qualified Restyled.JobLogLine as CW
import Restyled.Models hiding (fetchJobLog)
import Restyled.WebSockets
import Restyled.Widgets.JobLogLine

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = do
    cwLogs <-
        lift $ runDB $ maybe False ((== Just "__cw") . jobStdout) <$> get jobId

    let
        source = if cwLogs
            then stream
                (\mSince ->
                    lift
                        $ (,)
                        <$> runDB (fetchJobIsInProgress jobId)
                        <*> CW.fetchJobLogLines jobId mSince
                )
                (\mSince logLines -> getLastCreatedAt logLines <|> mSince)
                Nothing
            else stream
                (\offset ->
                    lift
                        $ runDB
                        $ (,)
                        <$> fetchJobIsInProgress jobId
                        <*> (entityVal <$$> fetchJobLogLines jobId offset)
                )
                (\offset -> (offset +) . length)
                0

    runConduit $ source .| mapMC (lift . formatJobLogLines) .| sinkWebSockets

getLastCreatedAt :: [JobLogLine] -> Maybe UTCTime
getLastCreatedAt = fmap (jobLogLineCreatedAt . NE.last) . NE.nonEmpty

formatJobLogLines :: [JobLogLine] -> Handler Text
formatJobLogLines logLines = do
    htmls <- traverse renderJobLogLine logLines
    pure $ TL.toStrict $ mconcat htmls

sinkWebSockets :: MonadIO m => ConduitT Text o (WebSocketsT m) ()
sinkWebSockets = await >>= \case
    Nothing -> lift $ sendClose @_ @Text ""
    Just x -> lift (sendTextDataAck x) >> sinkWebSockets

stream
    :: MonadIO m
    => (offset -> m (Bool, [item]))
    -> (offset -> [item] -> offset)
    -> offset
    -> ConduitT i [item] m ()
stream fetchLog getNextOffset = go 0
  where
    go backoff offset = do
        threadDelay $ backoff * 1000000

        (continue, items) <- lift $ fetchLog offset

        yield items

        let noItems = null items
            nextOffset = getNextOffset offset items
            nextBackoff = if noItems then min 5 (backoff + 1) else 0

        when (continue || not noItems) $ go nextBackoff nextOffset

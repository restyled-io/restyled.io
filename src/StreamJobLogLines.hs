module StreamJobLogLines
    ( streamJobLogLines
    )
where

import Import

import Network.WebSockets (ConnectionException)
import Widgets.JobLogLine
import Yesod.WebSockets

streamJobLogLines :: JobId -> WebSocketsT Handler ()
streamJobLogLines jobId = handle ignoreConnectionException $ go 0
  where
    go offset = do
        logLines <- lift $ runDB $ fetchJobLogLines jobId offset

        stop <- if null logLines
            then lift $ runDB $ not <$> fetchJobIsInProgress jobId
            else pure False

        unless stop $ do
            for_ logLines $ \logLine -> do
                html <- lift $ renderJobLogLine logLine
                sendTextData html
                void (receiveData @_ @Text)

            go $ offset + length logLines

fetchJobLogLines
    :: MonadIO m => JobId -> Int -> SqlPersistT m [Entity JobLogLine]
fetchJobLogLines jobId offset = selectList
    [JobLogLineJob ==. jobId]
    [Asc JobLogLineCreatedAt, OffsetBy offset]

fetchJobIsInProgress :: MonadIO m => JobId -> SqlPersistT m Bool
fetchJobIsInProgress jobId =
    isJust <$> selectFirst [JobId ==. jobId, JobCompletedAt ==. Nothing] []

ignoreConnectionException :: Applicative f => ConnectionException -> f ()
ignoreConnectionException _ = pure ()

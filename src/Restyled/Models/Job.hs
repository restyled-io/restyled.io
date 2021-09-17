module Restyled.Models.Job
    (
    -- * @'JobOutput'@
      captureJobLogLine
    , fetchLastJobLogLineCreatedAt

    -- * Temporary log-transition helpers
    , markJobAsCloudWatch
    , jobIsCloudWatch
    ) where

import Restyled.Prelude

import Restyled.Models.DB

captureJobLogLine :: MonadIO m => JobId -> Text -> Text -> SqlPersistT m ()
captureJobLogLine jobId stream content = do
    now <- liftIO getCurrentTime
    insert_ JobLogLine
        { jobLogLineJob = jobId
        , jobLogLineCreatedAt = now
        , jobLogLineStream = stream
        , jobLogLineContent = content
        }

fetchLastJobLogLineCreatedAt
    :: MonadIO m => JobId -> SqlPersistT m (Maybe UTCTime)
fetchLastJobLogLineCreatedAt jobId =
    jobLogLineCreatedAt . entityVal <$$> selectFirst
        [JobLogLineJob ==. jobId]
        [Desc JobLogLineCreatedAt]

markJobAsCloudWatch :: Job -> Job
markJobAsCloudWatch job = job { jobStdout = Just cwSigil }

jobIsCloudWatch :: Job -> Bool
jobIsCloudWatch = (== Just cwSigil) . jobStdout

cwSigil :: Text
cwSigil = "__cw"

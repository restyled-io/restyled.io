module Restyled.Backend.CompressJobs
    ( CompressOptions
    , compressOptions
    , runCompressJobs
    )
where

import Restyled.Prelude.Esqueleto

import Data.List (genericLength)
import Options.Applicative
import Restyled.Models

data CompressOptions = CompressOptions
    { coDays :: Natural
    , coLines :: Natural
    }

-- brittany-disable-next-binding

compressOptions :: Parser CompressOptions
compressOptions = CompressOptions
    <$> option auto
        ( short 'd'
        <> long "days"
        <> metavar "DAYS"
        <> help "Limit to completed more this many days ago"
        <> showDefault
        <> value 7
        )
    <*> option auto
        ( short 'l'
        <> long "lines"
        <> metavar "LINES"
        <> help "Limit to having at least this many lines of output"
        <> showDefault
        <> value 500
        )

runCompressJobs :: (HasLogFunc env, HasDB env) => CompressOptions -> RIO env ()
runCompressJobs CompressOptions {..} = do
    cutoff <- subtractDays (fromIntegral coDays) <$> getCurrentTime
    logInfo $ "Compressing jobs completed before " <> displayShow cutoff
    jobIds <- runDB $ fetchHeavyJobIds cutoff coLines

    concurrentlyWithProgress jobIds $ \(jobId, linesCount) n total -> do
        runDB $ compressJobOutput jobId
        logInfo
            $ fromString
            $ unpack
            $ "["
            <> tshow n
            <> "/"
            <> tshow total
            <> "]: "
            <> toPathPiece jobId
            <> ", "
            <> tshow linesCount
            <> " lines done"

fetchHeavyJobIds
    :: MonadIO m => UTCTime -> Natural -> SqlPersistT m [(JobId, Natural)]
fetchHeavyJobIds cutoff minLines =
    selectMap unValue2 . from $ \(jobs `InnerJoin` jobLogLines) -> do
        on $ jobLogLines ^. JobLogLineJob ==. jobs ^. persistIdField
        where_ $ not_ $ isNothing $ jobs ^. JobCompletedAt
        where_ $ jobs ^. JobCompletedAt <=. just (val cutoff)
        where_ $ isNothing $ jobs ^. JobStdout
        where_ $ isNothing $ jobs ^. JobStderr
        let linesCount = count $ jobLogLines ^. persistIdField
        groupBy $ jobs ^. persistIdField
        having $ linesCount >=. val minLines
        orderBy [desc linesCount, asc $ jobs ^. JobCreatedAt]
        pure (jobs ^. persistIdField, linesCount)

concurrentlyWithProgress
    :: MonadUnliftIO m => [a] -> (a -> Natural -> Natural -> m b) -> m ()
concurrentlyWithProgress xs f = pooledForConcurrentlyN_ 4 (zip xs [1 ..])
    $ \(x, n) -> f x n total
    where total = genericLength xs

subtractDays :: Int -> UTCTime -> UTCTime
subtractDays = addUTCTime . fromIntegral . negate . (* secondsPerDay)
    where secondsPerDay = 60 * 60 * 24

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

newtype CompressOptions = CompressOptions
    { coLimit :: Maybe Natural
    }

-- brittany-disable-next-binding

compressOptions :: Parser CompressOptions
compressOptions = CompressOptions
    <$> optional (option auto
        ( short 'L'
        <> long "limit"
        <> metavar "LIMIT"
        <> help "Limit how many Jobs are compressed"
        ))

runCompressJobs :: (HasLogFunc env, HasDB env) => CompressOptions -> RIO env ()
runCompressJobs CompressOptions {..} = do
    logInfo
        $ "Compressing"
        <> maybe "" ((" up to " <>) . displayShow) coLimit
        <> " job(s)"

    jobIds <- runDB $ fetchHeavyJobIds coLimit

    concurrentlyWithProgress jobIds $ \(jobId, linesCount) n total -> do
        mJob <- runDB $ do
            compressJobOutput jobId
            get jobId

        -- TODO: use real Approot and/or Route?
        let
            toJobUrl Job {..} =
                "https://restyled.io/gh/"
                    <> toPathPiece jobOwner
                    <> "/repos/"
                    <> toPathPiece jobRepo
                    <> "/jobs/"
                    <> toPathPiece jobId

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
            <> maybe "" ((": " <>) . toJobUrl) mJob

fetchHeavyJobIds
    :: MonadIO m => Maybe Natural -> SqlPersistT m [(JobId, Natural)]
fetchHeavyJobIds mLimit =
    selectMap unValue2 . from $ \(jobs `InnerJoin` jobLogLines) -> do
        on $ jobLogLines ^. JobLogLineJob ==. jobs ^. persistIdField
        where_ $ not_ $ isNothing $ jobs ^. JobCompletedAt
        where_ $ isNothing $ jobs ^. JobLog
        where_ $ isNothing $ jobs ^. JobStdout
        where_ $ isNothing $ jobs ^. JobStderr
        let linesCount = count $ jobLogLines ^. persistIdField
        groupBy $ jobs ^. persistIdField
        orderBy [desc linesCount, asc $ jobs ^. JobCreatedAt]
        for_ mLimit $ limit . fromIntegral
        pure (jobs ^. persistIdField, linesCount)

concurrentlyWithProgress
    :: MonadUnliftIO m => [a] -> (a -> Natural -> Natural -> m b) -> m ()
concurrentlyWithProgress xs f = pooledForConcurrentlyN_ 4 (zip xs [1 ..])
    $ \(x, n) -> f x n total
    where total = genericLength xs

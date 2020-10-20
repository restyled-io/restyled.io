module Restyled.Metrics
    ( JobMetrics(..)
    , fetchJobMetrics
    )
where

import Restyled.Prelude.Esqueleto

import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic
import qualified Database.Esqueleto as E
import Restyled.Models
import Restyled.TimeRange

data JobMetrics = JobMetrics
    { jmSucceeded :: Sum Int
    , jmFailed :: Sum Int
    , jmFailedUnknown :: Sum Int
    , jmUnfinished :: Sum Int
    , jmTotal :: Sum Int
    }
    deriving stock Generic

instance Semigroup JobMetrics where
    (<>) = gmappend

instance Monoid JobMetrics where
    mempty = gmempty

fetchJobMetrics :: MonadIO m => TimeRange -> SqlPersistT m JobMetrics
fetchJobMetrics range = selectFoldMap (convert . unValue5) $ from $ \jobs -> do
    let dateField =
            coalesceDefault [jobs ^. JobCompletedAt] (jobs ^. JobCreatedAt)
        caseExitCode check = case_
            [when_ (check $ jobs ^. JobExitCode) then_ $ val (1 :: Int)]
            (else_ $ val 0)

    where_ $ withinTimeRange dateField range

    pure
        ( count $ caseExitCode (==. just (val 0))
        , count
            $ caseExitCode (\c -> not_ (E.isNothing c) &&. c !=. just (val 0))
        , count $ caseExitCode (==. just (val 99))
        , count $ caseExitCode E.isNothing
        , countRows
        )
  where
    convert (succeeded, failed, unknown, unfinished, total) = JobMetrics
        { jmSucceeded = Sum succeeded
        , jmFailed = Sum failed
        , jmFailedUnknown = Sum unknown
        , jmUnfinished = Sum unfinished
        , jmTotal = Sum total
        }

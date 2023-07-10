module Restyled.Metrics
  ( JobMetrics (..)
  , fetchJobMetrics
  , buildJobMetrics

    -- * Re-exports
  , Sum (..)
  ) where

import Restyled.Prelude.Esqueleto

import qualified Data.Maybe as Maybe
import Data.Semigroup.Generic
import Restyled.Models
import Restyled.TimeRange

data JobMetrics = JobMetrics
  { jmSucceeded :: Sum Int
  , jmFailed :: Sum Int
  , jmFailedUnknown :: Sum Int
  , jmUnfinished :: Sum Int
  , jmTotal :: Sum Int
  }
  deriving stock (Generic)

instance Semigroup JobMetrics where
  (<>) = gmappend

instance Monoid JobMetrics where
  mempty = gmempty

fetchJobMetrics :: MonadIO m => TimeRange -> SqlPersistT m JobMetrics
fetchJobMetrics range = selectFoldMap (convert . unValue5) $ from $ \jobs -> do
  let
    dateField =
      coalesceDefault [jobs ^. JobCompletedAt] (jobs ^. JobCreatedAt)

    caseExitCode check =
      case_
        [when_ (check $ jobs ^. JobExitCode) then_ $ val (1 :: Int)]
        (else_ $ val 0)

    sumExitCode check = coalesceDefault [sum_ $ caseExitCode check] (val 0)

  where_ $ withinTimeRange dateField range

  pure
    ( sumExitCode (==. just (val 0))
    , sumExitCode (\c -> not_ (isNothing c) &&. c !=. just (val 0))
    , sumExitCode (==. just (val 99))
    , sumExitCode isNothing
    , countRows
    )
 where
  convert (succeeded, failed, unknown, unfinished, total) =
    JobMetrics
      { jmSucceeded = Sum succeeded
      , jmFailed = Sum failed
      , jmFailedUnknown = Sum unknown
      , jmUnfinished = Sum unfinished
      , jmTotal = Sum total
      }

buildJobMetrics :: [Entity Job] -> JobMetrics
buildJobMetrics = foldMap jobMetric

jobMetric :: Entity Job -> JobMetrics
jobMetric (Entity _ Job {..}) =
  JobMetrics
    { jmSucceeded = sumIfMaybe (== 0) jobExitCode
    , jmFailed = sumIfMaybe (/= 0) jobExitCode
    , jmFailedUnknown = sumIfMaybe (== 99) jobExitCode
    , jmUnfinished = sumIf $ Maybe.isNothing jobCompletedAt
    , jmTotal = 1
    }
 where
  sumIfMaybe :: (a -> Bool) -> Maybe a -> Sum Int
  sumIfMaybe p = sumIf . maybe False p

  sumIf :: Bool -> Sum Int
  sumIf x = if x then 1 else 0

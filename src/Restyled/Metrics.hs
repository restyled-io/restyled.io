{-# LANGUAGE QuasiQuotes #-}

module Restyled.Metrics
    ( JobMetrics(..)
    , fetchJobMetrics
    , JobMetricsByHour(..)
    , fetchJobMetricsByHour
    )
where

import Restyled.Prelude hiding (count)

import qualified Data.Map as Map
import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic
import Database.Persist.Sql (Single(..))
import Restyled.TimeRange
import Text.Shakespeare.Text (st)

data JobMetrics = JobMetrics
    { jmSucceeded :: Sum Int
    , jmFailed :: Sum Int
    , jmUnfinished :: Sum Int
    }
    deriving Generic

instance Semigroup JobMetrics where
    (<>) = gmappend

instance Monoid JobMetrics where
    mempty = gmempty

fromPair :: (Text, Int) -> JobMetrics
fromPair (status, count)
    | status == succeeded = JobMetrics (Sum count) 0 0
    | status == failed = JobMetrics 0 (Sum count) 0
    | status == unfinished = JobMetrics 0 0 (Sum count)
    | otherwise = JobMetrics 0 0 0

data JobMetricsByHour = JobMetricsByHour
    { jmbhEpoch :: Int
    , jmbhJobMetrics :: JobMetrics
    }

instance ToJSON JobMetricsByHour  where
    toJSON (JobMetricsByHour epoch JobMetrics {..}) = object
        [ "Date" .= epoch
        , succeeded .= getSum jmSucceeded
        , failed .= getSum jmFailed
        , unfinished .= getSum jmUnfinished
        ]

-- brittany-disable-next-binding

fetchJobMetrics :: MonadIO m => TimeRange -> SqlPersistT m JobMetrics
fetchJobMetrics range = fromRows <$> rawSqlWithTimeRange
    range
    [st|
            SELECT
                CASE
                    WHEN exit_code IS NULL THEN '#{unfinished}'
                    WHEN exit_code = 0 THEN '#{succeeded}'
                    ELSE '#{failed}'
                END,
                COUNT(*)
            FROM job
            WHERE #{dateField} >= ?
              AND #{dateField} <= ?
            GROUP BY 1
        |]
  where
    fromRows :: [(Single Text, Single Int)] -> JobMetrics
    fromRows = foldMap (fromPair . bimap unSingle unSingle)

-- brittany-disable-next-binding

fetchJobMetricsByHour
    :: MonadIO m
    => TimeRange
    -> SqlPersistT m [JobMetricsByHour]
fetchJobMetricsByHour range =
    fromRows <$> rawSqlWithTimeRange range
        [st|
            SELECT
                extract(epoch from date_trunc('hour', #{dateField})),
                CASE
                    WHEN exit_code IS NULL THEN '#{unfinished}'
                    WHEN exit_code = 0 THEN '#{succeeded}'
                    ELSE '#{failed}'
                END,
                COUNT(*)
            FROM job
            WHERE #{dateField} >= ?
              AND #{dateField} <= ?
            GROUP BY (1, 2)
        |]
  where
    fromRows :: [(Single Int, Single Text, Single Int)] -> [JobMetricsByHour]
    fromRows =
        map (uncurry JobMetricsByHour)
            . Map.toList
            . Map.fromListWith (<>)
            . map (regroup fromPair)

    regroup :: ((a, b) -> c) -> (Single x, Single a, Single b) -> (x, c)
    regroup f (Single x, Single a, Single b) = (x, f (a, b))

dateField :: Text
dateField = "COALESCE(completed_at, created_at)"

-- | Avoid typo-bugs by using this anywhere the textual value is needed
succeeded :: Text
succeeded = "Succeeded"

failed :: Text
failed = "Failed"

unfinished :: Text
unfinished = "Unfinished"

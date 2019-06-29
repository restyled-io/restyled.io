{-# LANGUAGE QuasiQuotes #-}

module Restyled.Metrics
    ( JobMetrics(..)
    , JobMetricsByHour(..)
    , fetchJobMetricsByHour
    )
where

import Restyled.Prelude hiding (count, to)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Sum(..))
import Data.Semigroup.Generic
import Database.Persist.Sql (Single(..), rawSql)
import Text.Shakespeare.Text (st)

data JobMetrics = JobMetrics
    { jmSucceeded :: Sum Int
    , jmFailed :: Sum Int
    , jmUnfinished :: Sum Int
    }
    deriving Generic

instance Semigroup JobMetrics where
    (<>) = gmappend

data JobMetricsByHour = JobMetricsByHour
    { jmbhEpoch :: Int
    , jmbhJobMetrics :: JobMetrics
    }

instance ToJSON JobMetricsByHour  where
    toJSON (JobMetricsByHour epoch JobMetrics {..}) = object
        [ "Date" .= epoch
        , "Succeeded" .= getSum jmSucceeded
        , "Failed" .= getSum jmFailed
        , "Unfinished" .= getSum jmUnfinished
        ]

-- brittany-disable-next-binding

fetchJobMetricsByHour
    :: MonadIO m
    => UTCTime -- ^ From
    -> UTCTime -- ^ To
    -> SqlPersistT m [JobMetricsByHour]
fetchJobMetricsByHour from to =
    fromRows <$> rawSql
        [st|
            SELECT
                extract(epoch from
                    date_trunc('hour', COALESCE(completed_at, created_at))
                ) AS epoch,
                CASE
                    WHEN exit_code IS NULL THEN 'Unfinished'
                    WHEN exit_code = 0 THEN 'Succeeded'
                    ELSE 'Failed'
                END AS status,
                COUNT(*) AS count
            FROM job
            WHERE COALESCE(completed_at, created_at) >= '#{show from}'
              AND COALESCE(completed_at, created_at) <= '#{show to}'
            GROUP BY (1, 2)
        |]
        []

fromRows :: [(Single Int, Single Text, Single Int)] -> [JobMetricsByHour]
fromRows = fromMap . Map.fromListWith (<>) . map fromRow
  where
    fromRow :: (Single Int, Single Text, Single Int) -> (Int, JobMetrics)
    fromRow (Single epoch, Single status, Single count) = case status of
        "Succeeded" -> (epoch, JobMetrics (Sum count) 0 0)
        "Failed" -> (epoch, JobMetrics 0 (Sum count) 0)
        "Unfinished" -> (epoch, JobMetrics 0 0 (Sum count))
        _ -> (epoch, JobMetrics 0 0 0)

    fromMap :: Map Int JobMetrics -> [JobMetricsByHour]
    fromMap = map (uncurry JobMetricsByHour) . Map.toList

{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Health
    ( runHealthChecks
    )
where

import Restyled.Prelude

import Data.List (genericLength)
import qualified Data.Text as T
import Restyled.Models
import Restyled.TimeRange

data Health = Normal | Warning | Fatal

data HealthCheck stat = HealthCheck
    { hcName :: Text
    , hcFilter :: [Entity Job] -> [Entity Job]
    , hcCompute :: [Entity Job] -> stat
    , hcHealth :: stat -> Health
    }

data HealthCheckResult stat = HealthCheckResult Health stat Text

runHealthChecks :: (HasLogFunc env, HasDB env) => RIO env ()
runHealthChecks = do
    range <- timeRangeFromMinutesAgo 60
    jobs <- runDB $ selectListWithTimeRange' JobCreatedAt range

    logHealthCheckResult $ runHealthCheck jobs $ HealthCheck
        { hcName = "Jobs completed last 10 minutes"
        , hcFilter = filterLastMinutes range 10
        , hcCompute = completions
        , hcHealth = \case
            0 -> Fatal
            _ -> Normal
        }

    logHealthCheckResult $ runHealthCheck jobs $ HealthCheck
        { hcName = "Effective success rate last 10 minutes"
        , hcFilter = filterLastMinutes range 10
        , hcCompute = errorRate [10, 11, 20]
        , hcHealth = thresholds (< 80) (< 95)
        }

    logHealthCheckResult $ runHealthCheck jobs $ HealthCheck
        { hcName = "Total success rate last 10 minutes"
        , hcFilter = filterLastMinutes range 10
        , hcCompute = errorRate []
        , hcHealth = thresholds (< 50) (< 65)
        }

    logHealthCheckResult $ runHealthCheck jobs $ HealthCheck
        { hcName = "Effective success rate last 60 minutes"
        , hcFilter = filterLastMinutes range 60
        , hcCompute = errorRate [10, 11, 20]
        , hcHealth = thresholds (< 85) (< 99)
        }

    logHealthCheckResult $ runHealthCheck jobs $ HealthCheck
        { hcName = "Total success rate last 60 minutes"
        , hcFilter = filterLastMinutes range 60
        , hcCompute = errorRate []
        , hcHealth = thresholds (< 70) (< 85)
        }

runHealthCheck :: [Entity Job] -> HealthCheck stat -> HealthCheckResult stat
runHealthCheck jobs HealthCheck {..} = HealthCheckResult
    (hcHealth stat)
    stat
    hcName
    where stat = hcCompute $ hcFilter jobs

logHealthCheckResult
    :: (HasLogFunc env, Show stat) => HealthCheckResult stat -> RIO env ()
logHealthCheckResult (HealthCheckResult health stat message) = case health of
    Normal -> logInfoN message'
    Warning -> logWarnN message'
    Fatal -> logErrorN message'
  where
    message' =
        T.unwords ["healthcheck=" <> tshow message, "stat=" <> tshow stat]

filterLastMinutes :: TimeRange -> Int -> [Entity Job] -> [Entity Job]
filterLastMinutes range =
    filterTimeRange (jobCreatedAt . entityVal) . subRangeToByMinutes range

completions :: [Entity Job] -> Int
completions = length . mapMaybe (jobExitCode . entityVal)

errorRate
    :: [Int] -- ^ Exit codes to ignore
    -> [Entity Job]
    -> Double
errorRate ignoreCodes jobs
    | total == 0 = 0
    | otherwise = (succeeded / total) * 100
  where
    exitCodes =
        filter (`notElem` ignoreCodes) $ mapMaybe (jobExitCode . entityVal) jobs
    total = genericLength exitCodes
    succeeded = genericLength $ filter (== 0) exitCodes

thresholds
    :: (stat -> Bool) -- ^ True if Fatal
    -> (stat -> Bool) -- ^ True if Warning
    -> stat
    -> Health
thresholds isFatal isWarning stat
    | isFatal stat = Fatal
    | isWarning stat = Warning
    | otherwise = Normal

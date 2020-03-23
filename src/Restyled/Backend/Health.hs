{-# LANGUAGE LambdaCase #-}

module Restyled.Backend.Health
    ( runHealthChecks
    )
where

import Restyled.Prelude

import Data.List (genericLength)
import qualified Data.Text as T
import Restyled.Backend.Webhook (queueDepth)
import Restyled.Models
import Restyled.TimeRange

data Stat = Count Int | Rate (Maybe Double)

showStat :: Stat -> String
showStat = \case
    Count c -> show c
    Rate Nothing -> "N/A"
    Rate (Just r) -> show r <> "%"

data Health = Normal | Warning | Fatal
    deriving Show

data HealthCheck m a = HealthCheck
    { hcName :: Text
    , hcFetch :: m a
    , hcCompute :: a -> Stat
    , hcHealth :: Stat -> Health
    }

runHealthChecks :: (HasLogFunc env, HasDB env, HasRedis env) => RIO env ()
runHealthChecks = do
    runHealthCheck HealthCheck
        { hcName = "Webhooks queue depth"
        , hcFetch = runRedis queueDepth
        , hcCompute = Count . maybe 999 fromIntegral
        , hcHealth = thresholds (> 50) (> 10)
        }

    runOnHoursHealthCheck HealthCheck
        { hcName = "Effective success rate last hour"
        , hcFetch = do
            range <- timeRangeFromMinutesAgo 60
            runDB $ selectListWithTimeRange JobCreatedAt range
        , hcCompute = errorRate [10, 11, 20]
        , hcHealth = thresholds (< 60) (< 80)
        }

runOnHoursHealthCheck :: HasLogFunc env => HealthCheck (RIO env) a -> RIO env ()
runOnHoursHealthCheck hc = do
    onHours <- fromMaybe True . isLocalWorkingHours "EST" <$> getCurrentTime

    if onHours
        then runHealthCheck hc
        else logInfoN $ "Skipping on-hours HealthCheck: " <> hcName hc

runHealthCheck :: HasLogFunc env => HealthCheck (RIO env) a -> RIO env ()
runHealthCheck HealthCheck {..} = do
    a <- hcFetch

    let stat = hcCompute a
        health = hcHealth stat
        message = T.pack $ unwords
            [ "healthcheck=" <> show hcName
            , "health=" <> show health
            , "stat=" <> showStat stat
            ]

    case health of
        Normal -> logInfoN message
        Warning -> logWarnN message
        Fatal -> logErrorN message

errorRate
    :: [Int] -- ^ Exit codes to ignore
    -> [Entity Job]
    -> Stat
errorRate ignoreCodes jobs
    | total == 0 = Rate Nothing
    | otherwise = Rate $ Just $ (succeeded / total) * 100
  where
    exitCodes =
        filter (`notElem` ignoreCodes) $ mapMaybe (jobExitCode . entityVal) jobs
    total = genericLength exitCodes
    succeeded = genericLength $ filter (== 0) exitCodes

thresholds
    :: (Double -> Bool) -- ^ True if Fatal
    -> (Double -> Bool) -- ^ True if Warning
    -> Stat
    -> Health
thresholds isFatal isWarning stat
    | threshold isFatal stat = Fatal
    | threshold isWarning stat = Warning
    | otherwise = Normal

threshold :: (Double -> Bool) -> Stat -> Bool
threshold f = \case
    Count c -> f $ fromIntegral c
    Rate Nothing -> False
    Rate (Just r) -> f r

isLocalWorkingHours :: String -> UTCTime -> Maybe Bool
isLocalWorkingHours tzString now = do
    tz <- readMaybe tzString
    let LocalTime {..} = utcToLocalTime tz now
    guard $ isWorkDay $ dayOfWeek localDay
    sevenAM <- makeTimeOfDayValid 7 0 0
    sevenPM <- makeTimeOfDayValid 19 0 0
    guard $ localTimeOfDay >= sevenAM && localTimeOfDay <= sevenPM
    pure True
  where
    isWorkDay = \case
        Sunday -> False
        Monday -> True
        Tuesday -> True
        Wednesday -> True
        Thursday -> True
        Friday -> True
        Saturday -> False

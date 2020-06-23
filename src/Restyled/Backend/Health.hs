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
        , hcCompute = errorRate [10, 11, 20, 25]
        , hcHealth = thresholds (< 60) (< 80)
        }

runOnHoursHealthCheck :: HasLogFunc env => HealthCheck (RIO env) a -> RIO env ()
runOnHoursHealthCheck hc = do
    localWorkingHours <- isLocalWorkingHours "EST" <$> getCurrentTime

    case localWorkingHours of
        UnableToReadTimeZone err -> do
            logWarnN $ "Error reading " <> pack err <> " as TimeZone"
            runHealthCheck hc
        UnableToMakeTimeOfDay h -> do
            logWarnN $ "Error making TimeOfDay for hour " <> tshow h
            runHealthCheck hc
        InsideWorkingHours -> runHealthCheck hc
        x -> logInfoN $ "Skipping " <> tshow (hcName hc) <> ": " <> tshow x

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

-- | Return value for local working hours check
--
-- We make separate contructors for programmer errors vs it being working hours
-- or not. This allows the caller to react accordingly (e.g. still run the
-- health-check for the "falsey" cases of programmer error), and gives a great
-- place to attach any supporting data (e.g. why we decided it wasn't working
-- hours).
--
-- Boolean-blindness is a thing, kids!
--
data LocalWorkingHours
    = UnableToReadTimeZone String
    | UnableToMakeTimeOfDay Int
    | NotWeekDay DayOfWeek
    | BeforeWorkingHours TimeOfDay
    | AfterWorkingHours TimeOfDay
    | InsideWorkingHours
    deriving Show

-- brittany-next-binding --columns=85

isLocalWorkingHours :: String -> UTCTime -> LocalWorkingHours
isLocalWorkingHours tzString now = either id id $ runExcept $ do
    tz <- noted (UnableToReadTimeZone tzString) $ readMaybe tzString
    let LocalTime {..} = utcToLocalTime tz now
    sevenAM <- noted (UnableToMakeTimeOfDay 7) $ makeTimeOfDayValid 7 0 0
    sevenPM <- noted (UnableToMakeTimeOfDay 19) $ makeTimeOfDayValid 19 0 0
    throwUnless (isWorkDay $ dayOfWeek localDay) $ NotWeekDay $ dayOfWeek localDay
    throwWhen (localTimeOfDay < sevenAM) $ BeforeWorkingHours localTimeOfDay
    throwWhen (localTimeOfDay > sevenPM) $ AfterWorkingHours localTimeOfDay
    pure InsideWorkingHours

isWorkDay :: DayOfWeek -> Bool
isWorkDay = \case
    Sunday -> False
    Monday -> True
    Tuesday -> True
    Wednesday -> True
    Thursday -> True
    Friday -> True
    Saturday -> False

noted :: e -> Maybe a -> Except e a
noted e = maybe (throwError e) pure

throwWhen :: MonadError e f => Bool -> e -> f ()
throwWhen p = when p . throwError

throwUnless :: MonadError e f => Bool -> e -> f ()
throwUnless p = unless p . throwError

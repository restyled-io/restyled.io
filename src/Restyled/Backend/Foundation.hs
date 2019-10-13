module Restyled.Backend.Foundation
    ( Backend(..)
    , loadBackend
    , runDB
    , runRedis
    )
where

import Restyled.Prelude

import Restyled.Settings
import Restyled.Settings.Display

-- | Like @'App'@ but with no webapp-related bits
data Backend = Backend
    { backendLogFunc :: LogFunc
    , backendSettings :: AppSettings
    , backendProcessContext :: ProcessContext
    , backendConnPool :: ConnectionPool
    , backendRedisConn :: Connection
    }

instance HasLogFunc Backend where
    logFuncL = lens backendLogFunc $ \x y -> x { backendLogFunc = y }

instance HasSettings Backend where
    settingsL = lens backendSettings $ \x y -> x { backendSettings = y }

instance HasProcessContext Backend where
    processContextL =
        lens backendProcessContext $ \x y -> x { backendProcessContext = y }

instance HasDB Backend where
    dbConnectionPoolL =
        lens backendConnPool $ \x y -> x { backendConnPool = y }

instance HasRedis Backend where
    redisConnectionL =
        lens backendRedisConn $ \x y -> x { backendRedisConn = y }

loadBackend :: AppSettings -> IO Backend
loadBackend settings@AppSettings {..} = do
    logFunc <- terminalLogFunc appLogLevel

    runRIO logFunc $ logInfoN $ pack $ displayAppSettings settings

    Backend logFunc settings
        <$> mkDefaultProcessContext
        <*> runRIO logFunc (createConnectionPool appDatabaseConf)
        <*> checkedConnect appRedisConf

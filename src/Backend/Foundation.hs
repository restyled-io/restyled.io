module Backend.Foundation
    ( Backend(..)
    , loadBackend
    , runDB
    , runRedis
    )
where

import Backend.Import

import Database.Persist.Postgresql (PostgresConf(..), createPostgresqlPool)
import Database.Redis (checkedConnect)
import RIO.Logger
import RIO.Orphans ()

-- | Like @'App'@ but with no webapp-related bits
data Backend = Backend
    { backendLogFunc :: LogFunc
    , backendSettings :: AppSettings
    , backendConnPool :: ConnectionPool
    , backendRedisConn :: Connection
    }

instance HasLogFunc Backend where
    logFuncL = lens backendLogFunc $ \x y -> x
        { backendLogFunc = y }

instance HasSettings Backend where
    settingsL = lens backendSettings $ \x y -> x
        { backendSettings = y }

instance HasDB Backend where
    dbConnectionPoolL = lens backendConnPool $ \x y -> x
        { backendConnPool = y }

instance HasRedis Backend where
    redisConnectionL = lens backendRedisConn $ \x y -> x
        { backendRedisConn = y }

loadBackend :: IO Backend
loadBackend = do
    settings@AppSettings {..} <- loadEnvSettings
    logFunc <- terminalLogFunc (loggerLogLevel appLogLevel)

    Backend logFunc settings
        <$> runRIO logFunc (createBackendPool appDatabaseConf)
        <*> checkedConnect appRedisConf

createBackendPool :: PostgresConf -> RIO LogFunc ConnectionPool
createBackendPool PostgresConf {..} = createPostgresqlPool pgConnStr pgPoolSize

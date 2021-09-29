{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.Backend.Foundation
    ( Backend(..)
    , loadBackend
    , loadBackendHandle
    , runDB
    , runRedis
    ) where

import Restyled.Prelude

import qualified Network.AWS as AWS (Env)
import Restyled.Queues
import Restyled.Settings

-- | Like @'App'@ but with no webapp-related bits
data Backend = Backend
    { backendLogFunc :: LogFunc
    , backendSettings :: AppSettings
    , backendProcessContext :: ProcessContext
    , backendConnPool :: ConnectionPool
    , backendRedisConn :: Connection
    , backendAWSEnv :: AWS.Env
    }

instance HasLogFunc Backend where
    logFuncL = lens backendLogFunc $ \x y -> x { backendLogFunc = y }

instance HasSettings Backend where
    settingsL = lens backendSettings $ \x y -> x { backendSettings = y }

instance HasQueues Backend where
    queuesL = settingsL . queuesL

instance HasProcessContext Backend where
    processContextL =
        lens backendProcessContext $ \x y -> x { backendProcessContext = y }

instance HasSqlPool Backend where
    sqlPoolL = lens backendConnPool $ \x y -> x { backendConnPool = y }

instance HasRedis Backend where
    redisConnectionL =
        lens backendRedisConn $ \x y -> x { backendRedisConn = y }

instance HasAWS Backend where
    awsEnvL = lens backendAWSEnv $ \x y -> x { backendAWSEnv = y }

instance HasAWS env => MonadAWS (RIO env) where
    liftAWS = implementAWS

loadBackend :: AppSettings -> IO Backend
loadBackend = loadBackendHandle stdout

loadBackendHandle :: Handle -> AppSettings -> IO Backend
loadBackendHandle h settings@AppSettings {..} = do
    logFunc <- terminalLogFunc h appLogLevel

    runRIO logFunc $ logInfo "Starting up..."
    let createPool = createConnectionPool appDatabaseConf appStatementTimeout

    Backend logFunc settings
        <$> mkDefaultProcessContext
        <*> runRIO logFunc createPool
        <*> checkedConnect appRedisConf
        <*> discoverAWS appAwsTrace

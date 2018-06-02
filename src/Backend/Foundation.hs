{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Backend.Foundation
    ( Backend(..)
    , MonadBackend
    , runBackend
    , runBackendHandler
    , runBackendApp
    , runRedis
    , module Control.Monad.Logger
    , module Database.Redis
    ) where

import Import

import Control.Monad.Logger
import Database.Persist.Sql (ConnectionPool)
import Database.Redis hiding (decode, runRedis)
import qualified Database.Redis as Redis

-- | Like @'App'@ but with no webapp-related bits
data Backend = Backend
    { backendSettings :: AppSettings
    , backendConnPool :: ConnectionPool
    , backendRedisConn :: Connection
    }

-- | Constraint synonym for backend actions' requirements
type MonadBackend m =
    ( MonadIO m
    , MonadLogger m
    , MonadReader Backend m
    , MonadUnliftIO m
    )

-- | Run a backend action
--
-- Uses supplied settings and connections, logs to stdout
--
runBackend :: MonadIO m => Backend -> ReaderT Backend (LoggingT m) a -> m a
runBackend b@Backend{..} f = runStdoutLoggingT
    $ filterLogger (const (backendSettings `allowsLevel`))
    $ runReaderT f b

-- | Run a backend action from a Handler (e.g. enqueuing a job)
--
-- Uses the @'App'@'s settings, connections, and logger
--
runBackendHandler :: ReaderT Backend (LoggingT Handler) a -> Handler a
runBackendHandler f = do
    app <- getYesod
    runBackendApp app f

-- | Extracted so @'runBackendTest'@ can use it in tests
runBackendApp :: App -> ReaderT Backend (LoggingT m) a -> m a
runBackendApp app@App{..} f = runLoggingT
    (runReaderT f Backend
        { backendSettings = appSettings
        , backendConnPool = appConnPool
        , backendRedisConn = appRedisConn
        }
    )
    (messageLoggerSource app appLogger)

-- | Run a @'Redis'@ action using the backend connection
runRedis :: MonadBackend m => Redis a -> m a
runRedis f = do
    conn <- asks backendRedisConn
    liftIO $ Redis.runRedis conn f

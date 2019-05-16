module Backend.Foundation
    ( Backend(..)
    , MonadBackend
    , runBackend
    , runBackendLogger
    , runDB
    , runRedis
    , module Control.Monad.Logger
    , module Database.Redis
    ) where

import Backend.Import

import Control.Monad.Logger
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Database.Redis hiding (Desc, decode, runRedis)
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
runBackend b@Backend {..} f = runBackendLogger backendSettings $ runReaderT f b

-- | Extracted for re-use when building the Postgres connection
runBackendLogger :: MonadIO m => AppSettings -> LoggingT m a -> m a
runBackendLogger settings =
    runStdoutLoggingT . filterLogger (const (settings `allowsLevel`))

-- | Run a @'SqlPersistT'@ action using the backend connection
runDB :: MonadBackend m => SqlPersistT m a -> m a
runDB action = do
    settings <- ask
    runSqlPool action $ backendConnPool settings

-- | Run a @'Redis'@ action using the backend connection
runRedis :: MonadBackend m => Redis a -> m a
runRedis f = do
    conn <- asks backendRedisConn
    liftIO $ Redis.runRedis conn f

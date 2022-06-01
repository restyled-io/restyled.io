module Restyled.DB
    ( HasSqlPool(..)
    , runDB
    , runDBUntraced

    -- * Construction
    , PostgresConf(..)
    , createConnectionPool

    -- * Re-export
    , ConnectionPool
    ) where

import Restyled.Prelude

import Data.Pool (withResource)
import Database.Persist.Postgresql
    (PostgresConf(..), createPostgresqlPool, createPostgresqlPoolModified)
import Database.Persist.Sql (ConnectionPool, runSqlConn, runSqlPool)
import Database.PostgreSQL.Simple (Connection, execute)
import Restyled.Tracing
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasSqlPool env where
    sqlPoolL :: Lens' env ConnectionPool

instance HasSqlPool ConnectionPool where
    sqlPoolL = id

instance HasSqlPool env => HasSqlPool (HandlerData child env) where
    sqlPoolL = envL . siteL . sqlPoolL

runDB
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasSqlPool env
       , HasTracingApp env
       , HasTransactionId env
       )
    => SqlPersistT m a
    -> m a
runDB action = do
    pool <- view sqlPoolL
    traceAppSegment "runDB" $ withRunInIO $ \runInIO ->
        withResource pool $ \backend ->
            runInIO $ runSqlConn action $ modifyBackend backend

runDBUntraced
    :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
    => SqlPersistT m a
    -> m a
runDBUntraced action = do
    pool <- view sqlPoolL
    runSqlPool action pool

modifyBackend :: SqlBackend -> SqlBackend
modifyBackend = id -- TODO: this is where we hook to log actual SQL segments

createConnectionPool
    :: (MonadUnliftIO m, MonadLoggerIO m)
    => PostgresConf
    -> Maybe Integer
    -> m ConnectionPool
createConnectionPool PostgresConf {..} = \case
    Nothing -> createPostgresqlPool pgConnStr pgPoolSize
    Just ms -> createPostgresqlPoolModified
        (setStatementTimeout ms)
        pgConnStr
        pgPoolSize

setStatementTimeout :: Integer -> Connection -> IO ()
setStatementTimeout ms conn =
    void $ execute conn "SET statement_timeout = ?" [ms]

module RIO.DB
    ( HasDB(..)
    , runDB

    -- * Construction
    , PostgresConf(..)
    , createConnectionPool

    -- * Re-export
    , SqlPersistT
    , ConnectionPool
    ) where

import RIO

import Control.Monad.Logger (MonadLogger)
import Database.Persist.Postgresql
    (PostgresConf(..), createPostgresqlPool, createPostgresqlPoolModified)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Database.PostgreSQL.Simple (Connection, execute)
import RIO.Orphans ()

class HasDB env where
    dbConnectionPoolL :: Lens' env ConnectionPool

runDB
    :: (MonadUnliftIO m, MonadReader env m, HasDB env) => SqlPersistT m a -> m a
runDB action = do
    pool <- view dbConnectionPoolL
    runSqlPool action pool

createConnectionPool
    :: (MonadUnliftIO m, MonadLogger m)
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

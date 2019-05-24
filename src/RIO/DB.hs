module RIO.DB
    ( HasDB(..)
    , runDB

    -- * Construction
    , PostgresConf(..)
    , createConnectionPool

    -- * Re-export
    , SqlPersistT
    , ConnectionPool
    )
where

import RIO

import Database.Persist.Postgresql (PostgresConf(..), createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import RIO.Orphans ()

class HasDB env where
    dbConnectionPoolL :: Lens' env ConnectionPool

runDB
    :: (HasDB env, MonadReader env m, MonadUnliftIO m) => SqlPersistT m a -> m a
runDB action = do
    pool <- view dbConnectionPoolL
    runSqlPool action pool

createConnectionPool :: HasLogFunc env => PostgresConf -> RIO env ConnectionPool
createConnectionPool PostgresConf {..} =
    createPostgresqlPool pgConnStr pgPoolSize

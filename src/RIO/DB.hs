module RIO.DB
    ( HasDB(..)
    , runDB

    -- * Re-export
    , SqlPersistT
    , ConnectionPool
    )
where

import RIO

import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)

class HasDB env where
    dbConnectionPoolL :: Lens' env ConnectionPool

runDB :: HasDB env => SqlPersistT (RIO env) a -> RIO env a
runDB action = do
    pool <- view dbConnectionPoolL
    runSqlPool action pool

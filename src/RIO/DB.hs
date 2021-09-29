module RIO.DB
    ( HasSqlPool(..)
    , runDB

    -- * Construction
    , PostgresConf(..)
    , createConnectionPool

    -- * Re-export
    , SqlPersistT
    , ConnectionPool
    ) where

import RIO

import Control.Monad.Logger (MonadLoggerIO)
import Database.Persist.Postgresql
    (PostgresConf(..), createPostgresqlPool, createPostgresqlPoolModified)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runSqlPool)
import Database.PostgreSQL.Simple (Connection, execute)
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasSqlPool env where
    sqlPoolL :: Lens' env ConnectionPool

instance HasSqlPool ConnectionPool where
    sqlPoolL = id

instance HasSqlPool env => HasSqlPool (HandlerData child env) where
    sqlPoolL = envL . siteL . sqlPoolL

runDB
    :: (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
    => SqlPersistT m a
    -> m a
runDB action = do
    pool <- view sqlPoolL
    runSqlPool action pool

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

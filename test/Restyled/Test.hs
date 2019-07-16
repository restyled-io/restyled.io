{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test
    ( withApp
    , runDB
    , module X
    )
where

import Restyled.Application as X ()
import Restyled.Foundation as X
import Restyled.Models as X
import Restyled.Prelude as X hiding (get, runDB)
import Restyled.Routes as X
import Restyled.Settings as X
import Restyled.Test.Authentication as X
import Restyled.Test.Expectations as X
import Restyled.Test.Yesod as X
import Test.QuickCheck as X

import qualified Data.Text as T
import Database.Persist.Sql
    (SqlBackend, SqlPersistT, connEscapeName, rawExecute, rawSql, unSingle)
import Database.Redis (del)
import LoadEnv (loadEnvFrom)
import Restyled.Backend.Foundation (loadBackend)
import Restyled.Backend.Job (queueName)
import Restyled.Backend.Webhook (webhookQueueName)
import Restyled.Cache
import qualified RIO.DB as RIO
import Text.Shakespeare.Text (st)

instance MonadCache (YesodExample site) where
    getCache _ = pure Nothing
    setCache _ _ = pure ()

-- | A monomorphic alias just to avoid annotations in specs
runDB :: HasDB env => SqlPersistT (YesodExample env) a -> YesodExample env a
runDB = RIO.runDB

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    loadEnvFrom ".env.test"
    foundation <- loadApp =<< loadBackend =<< loadSettings
    runRIO foundation $ do
        RIO.runDB wipeDB
        runRedis wipeRedis
    return (foundation, id)

wipeDB :: MonadIO m => SqlPersistT m ()
wipeDB = do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " <> T.intercalate ", " escapedTables
    rawExecute query []

wipeRedis :: Redis ()
wipeRedis = do
    void $ del [queueName]
    void $ del [webhookQueueName]

-- brittany-disable-next-binding

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = map unSingle <$> rawSql
    [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name <> 'installed_migrations';
    |] []

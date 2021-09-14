{-# LANGUAGE QuasiQuotes #-}

module Restyled.Test
    ( withApp
    , runDB
    , runRedis
    , module X
    ) where

import Restyled.Application as X ()
import Restyled.Foundation as X
import Restyled.Models as X
import Restyled.Prelude as X hiding (fieldLens, get, runDB, runRedis)
import Restyled.Routes as X
import Restyled.Settings as X
import Restyled.Test.Authentication as X
import Restyled.Test.Expectations as X
import Restyled.Test.Factories as X
import Restyled.Test.Lens as X
import Restyled.Test.Validate as X
import Restyled.Test.Yesod as X
import Test.QuickCheck as X

import qualified Data.Text as T
import Database.Persist.Sql (rawExecute, rawSql, unSingle)
import Database.Persist.Sql.Types.Internal (connEscapeRawName)
import Database.Redis (del, keys)
import LoadEnv (loadEnvFrom)
import qualified RIO.DB as RIO
import qualified RIO.Redis as RIO
import Restyled.Application (waiMiddleware)
import Restyled.Backend.Foundation (loadBackend)
import Text.Shakespeare.Text (st)

-- | A monomorphic alias just to avoid annotations in specs
runDB
    :: HasSqlPool env => SqlPersistT (YesodExample env) a -> YesodExample env a
runDB = RIO.runDB

-- | A monomorphic alias just to avoid annotations in specs
runRedis :: HasRedis env => Redis a -> YesodExample env a
runRedis = RIO.runRedis

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    loadEnvFrom ".env.test"
    foundation <- loadApp =<< loadBackend =<< loadSettings
    runRIO foundation $ do
        RIO.runDB wipeDB
        RIO.runRedis wipeRedis
    return (foundation, waiMiddleware foundation)

wipeDB :: MonadIO m => SqlPersistT m ()
wipeDB = do
    tables <- getTables
    escape <- asks connEscapeRawName

    let escapedTables = map (escapeWith escape . EntityNameDB) tables
        query = "TRUNCATE TABLE " <> T.intercalate ", " escapedTables
    rawExecute query []

wipeRedis :: Redis ()
wipeRedis = void $ runExceptT $ ExceptT . del =<< ExceptT (keys "restyled:*")

-- brittany-disable-next-binding

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = map unSingle <$> rawSql
    [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_name <> 'installed_migrations';
    |] []

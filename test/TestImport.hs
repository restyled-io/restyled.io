{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestImport
    ( YesodSpec
    , runDB
    , withApp
    , runTestRIO
    , authenticateAsUser
    , module X
    )
where

import Application (makeFoundation, makeLogWare)
import Backend.Job (queueName)
import Backend.Webhook (webhookQueueName)
import Cache
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Control.Monad.Fail (MonadFail(..))
import Data.Time as X
import Database.Persist as X hiding (get)
import Database.Persist.Sql
    (SqlBackend, SqlPersistT, connEscapeName, rawExecute, rawSql, unSingle)
import Database.Redis (del)
import Foundation as X
import Model as X
import RIO (RIO, runRIO)
import RIO.DB (HasDB)
import qualified RIO.DB as DB
import RIO.Orphans (HasResourceMap(..))
import RIO.Redis (Redis, runRedis)
import Routes as X
import Settings (AppSettings(..), loadEnvSettingsTest)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Lifted as X
import Test.HUnit (assertFailure)
import Text.Shakespeare.Text (st)
import Yesod.Core (MonadHandler(..))
import Yesod.Test as X hiding (YesodSpec)

type YesodSpec site = SpecM (TestApp site)

instance MonadCache (RIO App) where
    getCache _ = pure Nothing
    setCache _ _ = pure ()

instance HasResourceMap App where
    resourceMapL = error "resourceMapL used in test"

instance MonadHandler (RIO App) where
    type HandlerSite (RIO App) = ()
    type SubHandlerSite (RIO App) = ()

    liftHandler = error "liftHandler used in test"
    liftSubHandler = error "liftSubHandler used in test"

instance MonadFail (SIO s) where
    fail = liftIO . assertFailure

runDB :: HasDB env => SqlPersistT (RIO env) a -> YesodExample env a
runDB = runTestRIO . DB.runDB

runTestRIO :: RIO env a -> YesodExample env a
runTestRIO action = do
    app <- getTestYesod
    runRIO app action

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadEnvSettingsTest
    foundation <- makeFoundation settings
    runRIO foundation $ do
        DB.runDB wipeDB
        runRedis wipeRedis
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: MonadIO m => SqlPersistT m ()
wipeDB = do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
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

-- | Insert and authenticate as the given user
--
-- N.B. Only use this once (for a given @'User'@) per spec.
--
authenticateAsUser :: User -> YesodExample App ()
authenticateAsUser = authenticateAs <=< runDB . insertEntity

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    dummyLogin <- authPage "/dummy"

    request $ do
        setMethod "POST"
        addPostParam "ident" $ userCredsIdent u
        setUrl dummyLogin

authPage :: Text -> YesodExample App Text
authPage page = do
    testRoot <- fmap (appRoot . appSettings) getTestYesod
    return $ testRoot ++ "/auth/page" ++ page

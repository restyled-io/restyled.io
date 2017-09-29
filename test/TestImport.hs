{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import LoadEnv               (loadEnvFrom)
import Model                 as X
import Settings              (loadEnvSettings)
import Test.Hspec.Lifted     as X
import Text.Shakespeare.Text (st)
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)
import Yesod.Test            as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    loadEnvFrom ".env.test"
    settings <- loadEnvSettings
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

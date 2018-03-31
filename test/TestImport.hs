{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module TestImport
    ( runDB
    , withApp
    , runBackendTest
    , authenticateAsUser
    , postForm
    , authPage
    , module X
    ) where

import Application (makeFoundation, makeLogWare)
import Backend.Foundation (Backend, runBackendApp, runRedis)
import Backend.Job (queueName)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Control.Monad.Logger (LoggingT)
import Database.Persist as X hiding (get)
import Database.Persist.Sql
    ( SqlBackend
    , SqlPersistM
    , connEscapeName
    , rawExecute
    , rawSql
    , runSqlPersistMPool
    , unSingle
    )
import Database.Redis (del)
import Foundation as X
import LoadEnv (loadEnvFrom)
import Model as X
import Settings (AppSettings(..), loadEnvSettings)
import Test.Hspec.Lifted as X
import Text.Shakespeare.Text (st)
import Yesod.Core.Handler (RedirectUrl(..))
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

runBackendTest :: ReaderT Backend (LoggingT IO) a -> YesodExample App a
runBackendTest query = do
    app <- getTestYesod
    liftIO $ runBackendApp app query

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    loadEnvFrom ".env.test"
    settings <- loadEnvSettings
    foundation <- makeFoundation settings
    wipeDB foundation
    wipeRedis foundation
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

wipeRedis :: App -> IO ()
wipeRedis app = runBackendApp app $ runRedis $ void $ del [queueName]

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

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

-- | Post a CSRF-protected form
postForm
    :: (RedirectUrl App a, RedirectUrl App b)
    => a                -- ^ Page with the form
    -> b                -- ^ Page the form posts to
    -> [(Text, Text)]   -- ^ Field @Name => Value@s
    -> YesodExample App ()
postForm a b fs = do
    get a -- required to get a valid token

    request $ do
        setUrl b
        setMethod "POST"
        addToken
        for_ fs $ uncurry byLabel

authPage :: Text -> YesodExample App Text
authPage page = do
    testRoot <- fmap (appRoot . appSettings) getTestYesod
    return $ testRoot ++ "/auth/page" ++ page

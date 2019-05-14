{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestImport
    ( YesodSpec
    , runDB
    , withApp
    , runBackendTest
    , authenticateAsUser
    , module X
    ) where

import Application (makeFoundation, makeLogWare)
import Backend.Foundation (Backend, runBackendApp, runRedis)
import Backend.Job (queueName)
import Backend.Webhook (webhookQueueName)
import Cache
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Logger (LoggingT, NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Time as X
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
import Model as X
import Routes as X
import Settings (AppSettings(..), loadEnvSettingsTest)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Lifted as X
import Test.HUnit (assertFailure)
import Text.Shakespeare.Text (st)
import Yesod.Core (MonadHandler(..))
import Yesod.Test as X hiding (YesodSpec)

type YesodSpec site = SpecM (TestApp site)

instance MonadCache (NoLoggingT (ResourceT IO)) where
    getCache _ = pure Nothing
    setCache _ _ = pure ()

instance MonadHandler (NoLoggingT (ResourceT IO)) where
    type HandlerSite (NoLoggingT (ResourceT IO)) = ()
    type SubHandlerSite (NoLoggingT (ResourceT IO)) = ()

    liftHandler = error "liftHandler used in test"
    liftSubHandler = error "liftSubHandler used in test"

instance MonadFail (SIO s) where
    fail = liftIO . assertFailure

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
    settings <- loadEnvSettingsTest
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
wipeRedis app = runBackendApp app $ runRedis $ do
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

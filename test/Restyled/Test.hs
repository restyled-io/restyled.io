{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test
    ( YesodSpec
    , runDB
    , withApp
    , authenticateAsUser
    , module X
    )
where

import Restyled.Prelude as X hiding (get, runDB)

import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Logger (MonadLogger(..), toLogStr)
import qualified Data.Text as T
import Database.Persist.Sql
    (SqlBackend, SqlPersistT, connEscapeName, rawExecute, rawSql, unSingle)
import Database.Redis (del)
import LoadEnv (loadEnvFrom)
import Restyled.Application as X ()
import Restyled.Backend.Foundation (loadBackend)
import Restyled.Backend.Job (queueName)
import Restyled.Backend.Webhook (webhookQueueName)
import Restyled.Cache
import Restyled.Foundation as X
import Restyled.Models as X
import Restyled.Routes as X
import Restyled.Settings as X
import qualified RIO.DB as RIO
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.Lifted as X
import Test.HUnit (assertFailure)
import Test.QuickCheck as X
import Text.Shakespeare.Text (st)
import Yesod.Test as X hiding (YesodSpec)

type YesodSpec site = SpecM (TestApp site)

instance MonadCache (YesodExample App) where
    getCache _ = pure Nothing
    setCache _ _ = pure ()

instance HasLogFunc site => MonadLogger (YesodExample site) where
    monadLoggerLog loc source level msg = do
        logFunc <- view logFuncL
        liftIO $ logFuncLog logFunc loc source level $ toLogStr msg

instance MonadReader site (SIO (YesodExampleData site)) where
    ask = getTestYesod

    -- yesod-test doesn't expose enough of SIO to do this
    local = error "local used in test"

instance MonadFail (SIO s) where
    fail = liftIO . assertFailure

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
    testRoot <- fmap (appRoot . view settingsL) getTestYesod
    return $ testRoot <> "/auth/page" <> page

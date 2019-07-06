{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test
    ( YesodSpec
    , runDB
    , withApp
    , getWith
    , authenticateAsAdmin
    , authenticateAs
    , getTestAppAdmins
    , getBody
    , module X
    )
where

import Restyled.Prelude as X hiding (get, runDB)

import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Logger (MonadLogger(..), toLogStr)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Database.Persist.Sql
    (SqlBackend, SqlPersistT, connEscapeName, rawExecute, rawSql, unSingle)
import Database.Redis (del)
import LoadEnv (loadEnvFrom)
import Network.Wai.Test (SResponse(..))
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
import Yesod.Core (RedirectUrl)
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

-- | @GET@ with the given queyr parameters
getWith :: (RedirectUrl App url) => [(Text, Text)] -> url -> YesodExample App ()
getWith params route = request $ do
    setUrl route
    traverse_ (uncurry addGetParam) params

authenticateAsAdmin :: YesodExample App ()
authenticateAsAdmin = authenticateAs . NE.head =<< getTestAppAdmins

-- | Authenticate as the given Email
--
-- Ensures the dummy Plugin, and the email is treated as Ident. A @'User'@ is
-- inserted unless one exists already.
--
authenticateAs :: Text -> YesodExample App ()
authenticateAs email = do
    void $ runDB $ upsert
        User
            { userEmail = Just email
            , userGithubUserId = Nothing
            , userGithubUsername = Nothing
            , userGitlabUserId = Nothing
            , userGitlabUsername = Nothing
            , userGitlabAccessToken = Nothing
            , userGitlabRefreshToken = Nothing
            , userCredsIdent = email
            , userCredsPlugin = "dummy"
            }
        []

    testRoot <- getTestRoot

    request $ do
        setMethod "POST"
        addPostParam "ident" email
        setUrl $ testRoot <> "/auth/page/dummy"

getTestRoot :: YesodExample App Text
getTestRoot = appRoot . view settingsL <$> getTestYesod

getTestAppAdmins :: YesodExample App (NonEmpty Text)
getTestAppAdmins = fromMaybeM
    (expectationFailure' "Tests require configured Admins")
    (NE.nonEmpty . appAdmins . view settingsL <$> getTestYesod)

getBody :: YesodExample site LBS.ByteString
getBody = withResponse $ pure . simpleBody

expectationFailure' :: String -> YesodExample App a
expectationFailure' msg = expectationFailure msg >> error "never here"

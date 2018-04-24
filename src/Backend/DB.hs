{-# LANGUAGE FlexibleContexts #-}
-- |
--
-- This should ideally be in @"Backend.Foundation"@, but we put it here to avoid
-- constant clashes with @'Yesod.Persist.runDB'@. Modules which need this will
-- have to qualify or hide.
--
module Backend.DB
    ( runDB
    ) where

import Import hiding (runDB)

import Backend.Foundation
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)

runDB :: MonadBackend m => SqlPersistM a -> m a
runDB query = do
    pool <- asks backendConnPool
    liftIO $ runSqlPersistMPool query pool

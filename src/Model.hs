{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}
module Model
    ( module Model
    , module Model.Base
    ) where

import ClassyPrelude.Yesod

import Database.Persist.Quasi

import Model.Base

-- | A shorthand Context for working with Entities generically
--
-- > whatever :: DB m b r -> _ -> _ -> ReaderT b m (_)
--
type DB m backend record =
    ( MonadIO m
    , PersistEntity record
    , PersistEntityBackend record ~ BaseBackend backend
    , PersistUniqueWrite backend
    )

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- | Yes, we accept the race condition you expect
createOrUpdate :: DB m b r => r -> ReaderT b m (Key r)
createOrUpdate v = getByValue v >>= \case
    Just (Entity k _) -> k <$ replace k v
    Nothing -> insert v

-- | A verson of @'createOrUpdate'@ that returns a complete @'Entity'@
createOrUpdateEntity :: DB m b r => r -> ReaderT b m (Entity r)
createOrUpdateEntity v = Entity <$> createOrUpdate v <*> pure v

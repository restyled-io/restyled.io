{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model where

import ClassyPrelude.Yesod

import Database.Persist.Quasi
import GitHub.Data (Id, Name, Owner, PullRequest)
import qualified GitHub.Data as GH
import GitHub.Data.Apps (Installation)
import GitHub.Instances ()

type DB a = forall backend m.
    (backend ~ SqlBackend, MonadIO m) => ReaderT backend m a

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

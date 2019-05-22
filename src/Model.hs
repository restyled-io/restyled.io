{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model
    , module SVCS.Names
    , module SVCS.Payload
    ) where

import ClassyPrelude.Yesod

import Database.Persist.Quasi
import SVCS.Names
import SVCS.Payload

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

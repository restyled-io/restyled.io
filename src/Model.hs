{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model
    , module Model.PlanType
    , module SVCS.Names
    , module SVCS.Payload
    ) where

import ClassyPrelude.Yesod

import Database.Persist.Quasi
import Model.PlanType
import SVCS.Names
import SVCS.Payload

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

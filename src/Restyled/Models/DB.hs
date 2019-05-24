{-# LANGUAGE TemplateHaskell #-}

-- | Entities defined via @config/models@
module Restyled.Models.DB
    ( module Restyled.Models.DB
    )
where

import Restyled.Prelude

import Database.Persist.Quasi
import Database.Persist.TH

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

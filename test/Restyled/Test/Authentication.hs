{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Test.Authentication
  ( authenticateAs
  ) where

import Restyled.Prelude

import Restyled.Models
import Restyled.Test.Yesod
import Restyled.Yesod

authenticateAs :: Yesod site => Entity User -> YesodExample site ()
authenticateAs (Entity _ User {userCredsIdent, userCredsPlugin}) = do
  testRoot <- getTestRoot

  request $ do
    setMethod "POST"
    addPostParam "ident" userCredsIdent
    setUrl $ testRoot <> "/auth/page/" <> userCredsPlugin

{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Test.Authentication
    ( authenticateAs
    , authenticateAsAdmin
    , getTestAppAdmins
    ) where

import Restyled.Prelude

import qualified Data.List.NonEmpty as NE
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Test.Expectations
import Restyled.Test.Factories
import Restyled.Test.Graphula
import Restyled.Test.Yesod

authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ User { userCredsIdent, userCredsPlugin }) = do
    testRoot <- getTestRoot

    request $ do
        setMethod "POST"
        addPostParam "ident" userCredsIdent
        setUrl $ testRoot <> "/auth/page/" <> userCredsPlugin

authenticateAsAdmin
    :: (m ~ t (YesodExample App), MonadTrans t, GraphulaContext m '[User])
    => m (Entity User)
authenticateAsAdmin = do
    emails <- lift getTestAppAdmins
    user <- genUser (NE.head emails) id
    user <$ lift (authenticateAs user)

getTestAppAdmins :: YesodExample App (NonEmpty Text)
getTestAppAdmins = fromMaybeM
    (expectationFailure "Tests require configured Admins")
    (NE.nonEmpty . appAdmins . view settingsL <$> getTestYesod)

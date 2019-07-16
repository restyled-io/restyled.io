module Restyled.Test.Authentication
    ( authenticateAs
    , authenticateAsAdmin
    , getTestAppAdmins
    )
where

import Restyled.Prelude

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Test.Expectations
import Restyled.Test.Yesod

-- | Authenticate as the given Email
--
-- Ensures the dummy Plugin, and the email is treated as Ident. A @'User'@ is
-- inserted unless one exists already.
--
authenticateAs :: Text -> YesodExample App (Entity User)
authenticateAs email = do
    user <- runDB $ upsert
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

    pure user

authenticateAsAdmin :: YesodExample App (Entity User)
authenticateAsAdmin = authenticateAs . NE.head =<< getTestAppAdmins

getTestAppAdmins :: YesodExample App (NonEmpty Text)
getTestAppAdmins = fromMaybeM
    (expectationFailure "Tests require configured Admins")
    (NE.nonEmpty . appAdmins . view settingsL <$> getTestYesod)

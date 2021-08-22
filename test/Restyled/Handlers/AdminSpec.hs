module Restyled.Handlers.AdminSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Types.Header (hAuthorization)
import Restyled.ApiToken

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        -- Just a simple example page
        let getAdmin :: YesodExample App ()
            getAdmin = get $ AdminP $ AdminMachinesP AdminMachinesR

        it "404s for un-authenticated users" $ do
            getAdmin

            statusIs 404

        it "404s for un-authorized users" $ do
            void $ authenticateAs "normie@restyled.io"

            getAdmin

            statusIs 404

        it "allows authorized admins" $ do
            emails <- getTestAppAdmins

            for_ emails $ \email -> do
                void $ authenticateAs email

                getAdmin

                statusIs 200

        it "accepts token auth by the Authorization header for admins" $ do
            adminEmail <- NE.head <$> getTestAppAdmins
            adminToken <- runDB $ createUserWithToken adminEmail

            request $ do
                setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                addRequestHeader
                    ( hAuthorization
                    , "token " <> encodeUtf8 (apiTokenRaw adminToken)
                    )

            statusIs 200

        it "rejects token auth by the Authorization header for users" $ do
            userToken <- runDB $ createUserWithToken "x@example.com"

            request $ do
                setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                addRequestHeader
                    ( hAuthorization
                    , "token " <> encodeUtf8 (apiTokenRaw userToken)
                    )

            statusIs 404

        it "accepts token auth in a query parameter for admins" $ do
            adminEmail <- NE.head <$> getTestAppAdmins
            adminToken <- runDB $ createUserWithToken adminEmail

            request $ do
                setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                addGetParam "token" $ apiTokenRaw adminToken

            statusIs 200

        it "rejects token auth in a query parameter for users" $ do
            userToken <- runDB $ createUserWithToken "x@example.com"

            request $ do
                setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                addGetParam "token" $ apiTokenRaw userToken

            statusIs 404

createUserWithToken :: MonadIO m => Text -> SqlPersistT m ApiTokenRaw
createUserWithToken email = do
    userId <- insert User
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

    createApiToken userId "testing"

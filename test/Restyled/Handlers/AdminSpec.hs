module Restyled.Handlers.AdminSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Types.Header (hAuthorization)
import Restyled.ApiToken
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        -- Just a random example page
        let getAdmin :: YesodExample App ()
            getAdmin = get $ AdminP $ AdminMachinesP AdminMachinesR

        it "404s for un-authenticated users" $ do
            getAdmin

            statusIs 404

        it "404s for un-authorized users" $ graph $ do
            user <- genUser "normie@restyled.io" id
            lift $ do
                void $ authenticateAs user

                getAdmin

                statusIs 404

        it "allows authorized admins" $ graph $ do
            emails <- lift getTestAppAdmins
            users <- traverse (`genUser` id) emails

            lift $ for_ users $ \user -> do
                void $ authenticateAs user

                getAdmin

                statusIs 200

        it "accepts token auth by header for admins" $ graph $ do
            adminEmail <- lift $ NE.head <$> getTestAppAdmins
            adminToken <- createUserWithToken adminEmail

            lift $ do
                request $ do
                    setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                    addRequestHeader
                        ( hAuthorization
                        , "token " <> encodeUtf8 (apiTokenRaw adminToken)
                        )

                statusIs 200

        it "rejects token auth by header for users" $ graph $ do
            userToken <- createUserWithToken "x@example.com"

            lift $ do
                request $ do
                    setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                    addRequestHeader
                        ( hAuthorization
                        , "token " <> encodeUtf8 (apiTokenRaw userToken)
                        )

                statusIs 404

        it "accepts token auth in a query parameter for admins" $ graph $ do
            adminEmail <- lift $ NE.head <$> getTestAppAdmins
            adminToken <- createUserWithToken adminEmail

            lift $ do
                request $ do
                    setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                    addGetParam "token" $ apiTokenRaw adminToken

                statusIs 200

        it "rejects token auth in a query parameter for users" $ graph $ do
            userToken <- createUserWithToken "x@example.com"

            lift $ do
                request $ do
                    setUrl $ AdminP $ AdminMachinesP AdminMachinesR
                    addGetParam "token" $ apiTokenRaw userToken

                statusIs 404

createUserWithToken
    :: ( m ~ t (YesodExample site)
       , MonadTrans t
       , MonadIO m
       , GraphulaContext m '[User]
       , HasSqlPool site
       )
    => Text
    -> m ApiTokenRaw
createUserWithToken email = do
    user <-
        node @User ()
        $ edit
        $ (fieldLens UserEmail ?~ email)
        . (fieldLens UserCredsIdent .~ email)
        . (fieldLens UserCredsPlugin .~ "dummy")

    lift $ runDB $ createApiToken (entityKey user) "testing"

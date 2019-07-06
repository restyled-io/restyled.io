module Restyled.Handlers.AdminSpec
    ( spec
    )
where

import Restyled.Test

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        -- Just a simple example page
        let getAdmin = get $ AdminP $ AdminMachinesP AdminMachinesR

        it "404s for un-authenticated users" $ do
            getAdmin

            statusIs 404

        it "404s for un-authorized users" $ do
            authenticateAs "normie@restyled.io"

            getAdmin

            statusIs 404

        it "allows authorized admins" $ do
            emails <- getTestAppAdmins

            for_ emails $ \email -> do
                authenticateAs email

                getAdmin

                statusIs 200

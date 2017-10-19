{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.AdminSpec (spec) where

import           TestImport

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        it "turns away un-authenticated users" $ do
            get $ AdminP AdminSignupsR

            statusIs 403

        it "turns away un-authorized users" $ do
            let user = User
                    { userEmail = "normie@restyled.io"
                    , userCredsIdent = "1"
                    , userCredsPlugin = "dummy"
                    }
            entity <- runDB $ insertEntity user
            authenticateAs entity

            get $ AdminP AdminSignupsR

            statusIs 403

        -- N.B. .env.test is known to have an admin1 and admin2
        it "allows authorized users" $ do
            let user1 = User
                    { userEmail = "admin1@restyled.io"
                    , userCredsIdent = "1"
                    , userCredsPlugin = "dummy"
                    }
                user2 = User
                    { userEmail = "admin2@restyled.io"
                    , userCredsIdent = "2"
                    , userCredsPlugin = "dummy"
                    }
            entity1 <- runDB $ insertEntity user1
            entity2 <- runDB $ insertEntity user2

            authenticateAs entity1
            get $ AdminP AdminSignupsR
            statusIs 200

            authenticateAs entity2
            get $ AdminP AdminSignupsR
            statusIs 200

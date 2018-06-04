{-# LANGUAGE OverloadedStrings #-}
module Handler.AdminSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        it "directs un-authenticated users to log in" $ do
            githubForward <- encodeUtf8 <$> authPage "/github/forward"

            get $ AdminP AdminSignupsR

            statusIs 303
            assertHeader "Location" githubForward

        it "turns away un-authorized users" $ do
            authenticateAsUser User
                { userEmail = "normie@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }

            get $ AdminP AdminSignupsR

            statusIs 403

        -- N.B. .env.test is known to have an admin1 and admin2
        it "allows only authorized users" $ do
            authenticateAsUser User
                { userEmail = "admin1@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }
            get $ AdminP AdminSignupsR
            statusIs 200

            authenticateAsUser User
                { userEmail = "admin2@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userCredsIdent = "2"
                , userCredsPlugin = "dummy"
                }
            get $ AdminP AdminSignupsR
            statusIs 200

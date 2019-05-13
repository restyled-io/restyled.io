module Handler.AdminSpec
    ( spec
    )
where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        -- Just a simple example page
        let getAdmin = get $ AdminP $ AdminMachinesP AdminMachinesR

        it "404s for un-authenticated users" $ do
            getAdmin

            statusIs 404

        it "404s for un-authorized users" $ do
            authenticateAsUser User
                { userEmail = Just "normie@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabRefreshToken = Nothing
                , userGitlabAccessToken = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }

            getAdmin

            statusIs 404

        -- N.B. .env.test is known to have an admin1 and admin2
        it "allows only authorized users" $ do
            authenticateAsUser User
                { userEmail = Just "admin1@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabAccessToken = Nothing
                , userGitlabRefreshToken = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }
            getAdmin
            statusIs 200

            authenticateAsUser User
                { userEmail = Just "admin2@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabAccessToken = Nothing
                , userGitlabRefreshToken = Nothing
                , userCredsIdent = "2"
                , userCredsPlugin = "dummy"
                }
            getAdmin
            statusIs 200

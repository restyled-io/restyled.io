{-# LANGUAGE OverloadedStrings #-}

module Handler.AdminSpec
    ( spec
    ) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "AdminP" $ do
        it "404s for un-authenticated users" $ do
            get $ AdminP $ AdminPlansP AdminPlansR

            statusIs 404

        it "404s for un-authorized users" $ do
            authenticateAsUser User
                { userEmail = "normie@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabRefreshToken = Nothing
                , userGitlabAccessToken = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }

            get $ AdminP $ AdminPlansP AdminPlansR

            statusIs 404

        -- N.B. .env.test is known to have an admin1 and admin2
        it "allows only authorized users" $ do
            authenticateAsUser User
                { userEmail = "admin1@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabAccessToken = Nothing
                , userGitlabRefreshToken = Nothing
                , userCredsIdent = "1"
                , userCredsPlugin = "dummy"
                }
            get $ AdminP $ AdminPlansP AdminPlansR
            statusIs 200

            authenticateAsUser User
                { userEmail = "admin2@restyled.io"
                , userGithubUserId = Nothing
                , userGithubUsername = Nothing
                , userGitlabUserId = Nothing
                , userGitlabUsername = Nothing
                , userGitlabAccessToken = Nothing
                , userGitlabRefreshToken = Nothing
                , userCredsIdent = "2"
                , userCredsPlugin = "dummy"
                }
            get $ AdminP $ AdminPlansP AdminPlansR
            statusIs 200

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.AuthenticationSpec
    ( spec
    )
where

import Restyled.Test

import qualified Data.Text as T
import Restyled.Authentication
import Restyled.Yesod

instance Eq (AuthenticationResult App) where
    Authenticated a == Authenticated b = a == b
    ServerError a == ServerError b = a == b
    UserError a == UserError b = defaultMessage a == defaultMessage b
    _ == _ = False

instance Show (AuthenticationResult App) where
    show (Authenticated userId) = "Authenticated: " <> show userId
    show (ServerError msg) = "ServerError: " <> show msg
    show (UserError msg) = unpack ("UserError: " <> defaultMessage msg)

spec :: Spec
spec = withApp $ do
    describe "authenticateUser" $ do
        context "without OAuth2 data in credsExtra" $ do
            let creds :: Creds App
                creds = Creds "github" "1" []

            it "accepts a known user" $ do
                (userId, result) <-
                    runDB
                    $ (,)
                    <$> insert (someUser creds "pat@example.com")
                    <*> authenticateUser creds

                result `shouldBe` Authenticated userId

            it "rejects unknown users" $ do
                result <- runDB $ authenticateUser creds
                result `shouldSatisfy` isUserError

        context "with incorrect OAuth2 data in credsExtra" $ do
            let creds :: Creds App
                creds = Creds "github" "1" [("userResponse", "{}")]

            it "accepts a known user" $ do
                (userId, result) <-
                    runDB
                    $ (,)
                    <$> insert (someUser creds "pat@example.com")
                    <*> authenticateUser creds

                result `shouldBe` Authenticated userId

            it "rejects unknown users" $ do
                result <- runDB $ authenticateUser creds
                result `shouldSatisfy` isUserError

        context "with OAuth2 data in credsExtra" $ do
            let userResponse :: Text
                userResponse = T.unlines
                    [ "{"
                    , "  \"id\": 1,"
                    , "  \"email\": \"me@example.com\","
                    , "  \"login\": \"pbrisbin\""
                    , "}"
                    ]

                creds :: Creds App
                creds = Creds "github" "1" [("userResponse", userResponse)]

            it "creates a new user" $ do
                (result, mUser) <-
                    runDB
                    $ (,)
                    <$> authenticateUser creds
                    <*> selectFirst [UserCredsPlugin ==. "github"] []

                Just result `shouldBe` (authenticatedAs <$> mUser)

                (entityVal <$> mUser) `shouldBe` Just (emptyUser creds)
                    { userEmail = Just "me@example.com"
                    , userGithubUserId = Just 1
                    , userGithubUsername = Just "pbrisbin"
                    }

            it "updates an existing user" $ do
                (userId, result, mUser) <-
                    runDB
                    $ (,,)
                    <$> insert (someUser creds "pat@example.com")
                    <*> authenticateUser creds
                    <*> selectFirst [UserCredsPlugin ==. "github"] []

                result `shouldBe` Authenticated userId

                (entityKey <$> mUser) `shouldBe` Just userId
                (entityVal <$> mUser) `shouldBe` Just (emptyUser creds)
                    { userEmail = Just "me@example.com"
                    , userGithubUserId = Just 1
                    , userGithubUsername = Just "pbrisbin"
                    }

authenticatedAs :: Entity User -> AuthenticationResult App
authenticatedAs = Authenticated . entityKey

isUserError :: AuthenticationResult site -> Bool
isUserError (UserError _) = True
isUserError _ = False

someUser :: Creds site -> Text -> User
someUser creds email = (emptyUser creds) { userEmail = Just email }

emptyUser :: Creds site -> User
emptyUser Creds {..} = User
    { userEmail = Nothing
    , userGithubUserId = Nothing
    , userGithubUsername = Nothing
    , userGitlabUserId = Nothing
    , userGitlabUsername = Nothing
    , userGitlabAccessToken = Nothing
    , userGitlabRefreshToken = Nothing
    , userCredsIdent = credsIdent
    , userCredsPlugin = credsPlugin
    }

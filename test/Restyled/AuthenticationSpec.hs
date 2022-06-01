{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.AuthenticationSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.Text as T
import qualified Prelude as Unsafe
import Restyled.Authentication
import Restyled.Test.Graphula
import Restyled.Yesod

instance Eq (AuthenticationResult App) where
    Authenticated a == Authenticated b = a == b
    ServerError a == ServerError b = a == b
    UserError a == UserError b = defaultMessage a == defaultMessage b
    _ == _ = False

instance Show (AuthenticationResult App) where
    show (Authenticated userId) = "Authenticated: " <> show @String userId
    show (ServerError msg) = "ServerError: " <> show @String msg
    show (UserError msg) = unpack ("UserError: " <> defaultMessage msg)

spec :: Spec
spec = withApp $ do
    describe "authenticateUser" $ do
        context "without OAuth2 data in credsExtra" $ do
            let creds :: Creds App
                creds = Creds "github" "1" []

            it "accepts a known user" $ graph $ do
                user <-
                    node @User ()
                    $ edit
                    $ (fieldLens UserCredsIdent .~ "1")
                    . (fieldLens UserCredsPlugin .~ "github")

                result <- lift $ runDB $ authenticateUser creds

                result `shouldBe` Authenticated (entityKey user)

            it "rejects unknown users" $ do
                result <- runDB $ authenticateUser creds
                result `shouldSatisfy` isUserError

        context "with incorrect OAuth2 data in credsExtra" $ do
            let creds :: Creds App
                creds = Creds "github" "1" [("userResponse", "{}")]

            it "accepts a known user" $ graph $ do
                user <-
                    node @User ()
                    $ edit
                    $ (fieldLens UserCredsIdent .~ "1")
                    . (fieldLens UserCredsPlugin .~ "github")

                result <- lift $ runDB $ authenticateUser creds

                result `shouldBe` Authenticated (entityKey user)

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

            it "creates a new user" $ runDB $ do
                result <- authenticateUser creds

                Just (Entity userId User {..}) <- selectFirst
                    [UserCredsPlugin ==. "github"]
                    []
                result `shouldBe` Authenticated userId
                userEmail `shouldBe` Just "me@example.com"
                userGithubUserId `shouldBe` Just 1
                userGithubUsername `shouldBe` Just "pbrisbin"

            it "updates an existing user" $ graph $ do
                user <-
                    node @User ()
                    $ edit
                    $ (fieldLens UserCredsIdent .~ "1")
                    . (fieldLens UserCredsPlugin .~ "github")
                lift $ runDB $ do
                    result <- authenticateUser creds

                    result `shouldBe` Authenticated (entityKey user)

                    Just (Entity userId User {..}) <- selectFirst
                        [UserCredsPlugin ==. "github"]
                        []
                    userId `shouldBe` entityKey user
                    userEmail `shouldBe` Just "me@example.com"
                    userGithubUserId `shouldBe` Just 1
                    userGithubUsername `shouldBe` Just "pbrisbin"

isUserError :: AuthenticationResult site -> Bool
isUserError (UserError _) = True
isUserError _ = False

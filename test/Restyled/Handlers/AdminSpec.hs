module Restyled.Handlers.AdminSpec
  ( spec
  ) where

import Restyled.Test

import Network.HTTP.Types.Header (hAuthorization)
import Restyled.ApiToken
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
  describe "AdminP" $ do
    -- Just a random example page
    let
      adminRoute = AdminP $ AdminOffersP AdminOffersR

      getAdmin :: YesodExample App ()
      getAdmin = get adminRoute

    it "404s for un-authenticated users" $ do
      getAdmin

      statusIs 404

    it "404s for un-authorized users" $ graph $ do
      user <- genUser "normie@restyled.io" id
      lift $ do
        authenticateAs user

        getAdmin

        statusIs 404

    it "allows authorized admins" $ graph $ do
      admin <- genAdmin
      lift $ do
        authenticateAs admin

        getAdmin

        statusIs 200

    it "accepts token auth by header for admins" $ graph $ do
      admin <- genAdmin
      lift $ do
        adminToken <- runDB $ createApiToken (entityKey admin) "testing"

        request $ do
          setUrl adminRoute
          addRequestHeader
            ( hAuthorization
            , "token " <> encodeUtf8 (apiTokenRaw adminToken)
            )

        statusIs 200

    it "rejects token auth by header for users" $ graph $ do
      user <- genUser "x@example.com" id
      lift $ do
        userToken <- runDB $ createApiToken (entityKey user) "testing"

        request $ do
          setUrl adminRoute
          addRequestHeader
            ( hAuthorization
            , "token " <> encodeUtf8 (apiTokenRaw userToken)
            )

        statusIs 404

    it "accepts token auth in a query parameter for admins" $ graph $ do
      admin <- genAdmin
      lift $ do
        adminToken <- runDB $ createApiToken (entityKey admin) "testing"

        request $ do
          setUrl adminRoute
          addGetParam "token" $ apiTokenRaw adminToken

        statusIs 200

    it "rejects token auth in a query parameter for users" $ graph $ do
      user <- genUser "x@example.com" id
      lift $ do
        userToken <- runDB $ createApiToken (entityKey user) "testing"

        request $ do
          setUrl adminRoute
          addGetParam "token" $ apiTokenRaw userToken

        statusIs 404

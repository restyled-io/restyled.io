module Restyled.Handlers.WebhooksSpec
  ( spec
  ) where

import Restyled.Test

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Restyled.Redis (brpop)
import System.FilePath ((</>))

spec :: Spec
spec = withApp $ do
  describe "POST /webhooks" $ do
    it "stores the body and responds 201" $ do
      let
        q :: ByteString
        q = "restyled:agent:webhooks"
      body <- readFixture "webhooks/github/pull-request-opened.json"

      request $ do
        setUrl WebhooksR
        setMethod "POST"
        setRequestBody body

      statusIs 201
      runRedis (brpop [q] 3)
        `shouldReturn` Right (Just (q, LB.toStrict body))

readFixture :: MonadIO m => FilePath -> m LB.ByteString
readFixture = liftIO . L8.readFile . ("fixtures" </>)

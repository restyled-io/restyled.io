{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test.Yesod
    ( YesodSpec
    , getWith
    , getBody
    , getTestRoot
    , module X
    )
where

import Restyled.Prelude

import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Logger (MonadLogger(..), toLogStr)
import qualified Data.ByteString.Lazy as LBS
import Network.Wai.Test (SResponse(..))
import Restyled.Cache as X
import Restyled.Test.Expectations
import Test.Hspec.Core.Spec (SpecM)
import Yesod.Core
import Yesod.Test as X hiding (YesodSpec)

type YesodSpec site = SpecM (TestApp site)

instance HasLogFunc site => MonadLogger (YesodExample site) where
    monadLoggerLog loc source level msg = do
        logFunc <- view logFuncL
        liftIO $ logFuncLog logFunc loc source level $ toLogStr msg

instance MonadReader site (SIO (YesodExampleData site)) where
    ask = getTestYesod
    local _ _ = expectationFailure "local cannot be used in a test"

instance HasRedis site => MonadCache (SIO (YesodExampleData site)) where
    getCache = getCache'
    setCache = setCache'

instance MonadFail (SIO s) where
    fail = expectationFailure

-- | @GET@ with the given query parameters
getWith
    :: (RedirectUrl site url, Yesod site)
    => [(Text, Text)]
    -> url
    -> YesodExample site ()
getWith params route = request $ do
    setUrl route
    traverse_ (uncurry addGetParam) params

-- | Get the raw response body
--
-- Hint: @"Data.Aeson.Lens"@ has an instance for operating directly on this.
--
getBody :: YesodExample site LBS.ByteString
getBody = withResponse $ pure . simpleBody

-- | Get the test application's root as a @'Text'@
--
-- Errors if using @'ApprootRequest'@ which we can't make use of for this.
--
getTestRoot :: forall site . Yesod site => YesodExample site Text
getTestRoot = do
    site <- getTestYesod

    case approot @site of
        ApprootRelative -> pure ""
        ApprootStatic r -> pure r
        ApprootMaster f -> pure $ f site
        ApprootRequest _ ->
            expectationFailure "getTestRoot can't be used with ApprootRequest"

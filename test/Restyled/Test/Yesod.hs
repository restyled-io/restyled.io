{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test.Yesod
    ( YesodSpec
    , getTestRoot
    , putJSON
    , getJsonBody
    , module X
    ) where

import Restyled.Prelude

import Control.Monad.Logger (MonadLogger(..), toLogStr)
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
    getCache = getCacheRedis
    setCache = setCacheRedis

instance MonadFail (SIO s) where
    fail = expectationFailure

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

putJSON
    :: (RedirectUrl site url, Yesod site, ToJSON body)
    => url
    -> body
    -> YesodExample site ()
putJSON route body = request $ do
    setUrl route
    setMethod "PUT"
    addRequestHeader ("Accept", "application/json")
    addRequestHeader ("Content-type", "application/json")
    setRequestBody $ encode body

getJsonBody :: YesodExample site Value
getJsonBody = do
    mResp <- getResponse

    case mResp of
        Nothing -> expectationFailure "No response"
        Just resp -> do
            let body = simpleBody resp
            case eitherDecode body of
                Left err ->
                    expectationFailure
                        $ "Body did not parse as JSON"
                        <> "\n  Errors: "
                        <> err
                        <> "\n  Body:   "
                        <> show body
                Right v -> pure v

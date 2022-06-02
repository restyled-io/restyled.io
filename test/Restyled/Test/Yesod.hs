{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test.Yesod
    ( YesodSpec
    , getTestRoot
    , patchJSON
    , putJSON
    , getJsonBody
    , followRedirect
    , module X
    ) where

import Restyled.Prelude

import qualified Control.Monad.State as State
import Network.Wai.Test (SResponse(..))
import Restyled.Cache as X
import Restyled.Settings
import Restyled.Test.Expectations
import Restyled.Tracing
import Test.Hspec.Core.Spec (SpecM)
import Yesod.Core
import Yesod.Test as X hiding (YesodSpec, followRedirect)
import qualified Yesod.Test as Yesod

type YesodSpec site = SpecM (TestApp site)

instance HasSettings site => HasSettings (YesodExampleData site) where
    settingsL = siteL . settingsL

instance HasLogger site => HasLogger (YesodExampleData site) where
    loggerL = siteL . loggerL

instance HasRedis site => HasRedis (YesodExampleData site) where
    redisConnectionL = siteL . redisConnectionL

instance HasTracingApp site => HasTracingApp (YesodExampleData site) where
    tracingAppL = siteL . tracingAppL

instance HasTransactionId (YesodExampleData site) where
    transactionIdL = lens (const Nothing) const

instance HasSqlPool site => HasSqlPool (YesodExampleData site) where
    sqlPoolL = siteL . sqlPoolL

siteL :: Lens' (YesodExampleData site) site
siteL = lens yedSite $ \x y -> x { yedSite = y }

instance HasLogger site => MonadLogger (YesodExample site) where
    monadLoggerLog loc source level msg = do
        site <- ask
        runLoggerLoggingT site $ monadLoggerLog loc source level msg

instance (HasRedis site, HasTracingApp site) => MonadCache (YesodExample site) where
    getCache = getCacheRedis
    setCache = setCacheRedis

instance MonadReader s (SIO s) where
    ask = State.get
    local f m = do
        s <- State.get
        State.modify f
        result <- m
        result <$ State.put s

instance MonadFail (SIO s) where
    fail = expectationFailure

followRedirect :: (HasCallStack, Yesod site) => YesodExample site ()
followRedirect = do
    result <- Yesod.followRedirect
    either (expectationFailure . unpack) (const $ pure ()) result

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

patchJSON
    :: (RedirectUrl site url, Yesod site, ToJSON body)
    => url
    -> body
    -> YesodExample site ()
patchJSON route body = request $ do
    setUrl route
    setMethod "PATCH"
    addRequestHeader ("Accept", "application/json")
    addRequestHeader ("Content-type", "application/json")
    setRequestBody $ encode body

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

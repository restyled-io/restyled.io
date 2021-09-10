{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test.Yesod
    ( YesodSpec
    , getTestRoot
    , patchJSON
    , putJSON
    , getJsonBody
    , module X
    ) where

import Restyled.Prelude

import Control.Monad.Logger (MonadLogger(..), toLogStr)
import qualified Control.Monad.State as State
import Network.Wai.Test (SResponse(..))
import Restyled.Cache as X
import Restyled.Settings
import Restyled.Test.Expectations
import Test.Hspec.Core.Spec (SpecM)
import Yesod.Core
import Yesod.Test as X hiding (YesodSpec)

type YesodSpec site = SpecM (TestApp site)

instance HasSettings site => HasSettings (YesodExampleData site) where
    settingsL = siteL . settingsL

instance HasLogFunc site => HasLogFunc (YesodExampleData site) where
    logFuncL = siteL . logFuncL

instance HasProcessContext site => HasProcessContext (YesodExampleData site) where
    processContextL = siteL . processContextL

instance HasRedis site => HasRedis (YesodExampleData site) where
    redisConnectionL = siteL . redisConnectionL

instance HasSqlPool site => HasSqlPool (YesodExampleData site) where
    sqlPoolL = siteL . sqlPoolL

siteL :: Lens' (YesodExampleData site) site
siteL = lens yedSite $ \x y -> x { yedSite = y }

instance HasLogFunc site => MonadLogger (YesodExample site) where
    monadLoggerLog loc source level msg = do
        logFunc <- view logFuncL
        liftIO $ logFuncLog logFunc loc source level $ toLogStr msg

instance HasRedis site => MonadCache (YesodExample site) where
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

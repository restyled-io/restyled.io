{-# OPTIONS_GHC -fno-warn-orphans #-}

module RIO.Handler () where

import RIO

import RIO.DB
import RIO.Process
import RIO.Redis
import Yesod.Core.Types (HandlerData(..), RunHandlerEnv(..))

instance HasLogFunc env => HasLogFunc (HandlerData child env) where
    logFuncL = handlerEnvL . siteL . logFuncL

instance HasProcessContext env => HasProcessContext (HandlerData child env) where
    processContextL = handlerEnvL . siteL . processContextL

instance HasDB env => HasDB (HandlerData child env) where
    dbConnectionPoolL = handlerEnvL . siteL . dbConnectionPoolL

instance HasRedis env => HasRedis (HandlerData child env) where
    redisConnectionL = handlerEnvL . siteL . redisConnectionL

handlerEnvL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
handlerEnvL = lens handlerEnv $ \x y -> x { handlerEnv = y }

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x { rheSite = y }

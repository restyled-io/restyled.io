module Yesod.Lens
    ( handlerEnvL
    , siteL
    ) where

import RIO

import Yesod.Core.Types (HandlerData(..), RunHandlerEnv(..))

handlerEnvL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
handlerEnvL = lens handlerEnv $ \x y -> x { handlerEnv = y }

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x { rheSite = y }

module Yesod.Core.Types.Lens
    ( envL
    , siteL
    ) where

import RIO

import Yesod.Core.Types (HandlerData(..), RunHandlerEnv(..))

envL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
envL = lens handlerEnv $ \x y -> x { handlerEnv = y }

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x { rheSite = y }

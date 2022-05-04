module Yesod.Core.Types.Lens
    (
    -- * 'HandlerData'
      requestL
    , envL

    -- * 'RunHandlerEnv'
    , siteL

    -- * 'YesodRequest'
    , waiRequestL

    -- * 'Request'
    , headersL
    , headerL
    ) where

import RIO

import Network.HTTP.Types.Header (Header, HeaderName)
import Network.Wai (Request(..), Response, mapResponseHeaders, responseHeaders)
import Yesod.Core.Types (HandlerData(..), RunHandlerEnv(..), YesodRequest(..))

requestL :: Lens' (HandlerData child site) YesodRequest
requestL = lens handlerRequest $ \x y -> x { handlerRequest = y }

envL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
envL = lens handlerEnv $ \x y -> x { handlerEnv = y }

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x { rheSite = y }

waiRequestL :: Lens' YesodRequest Request
waiRequestL = lens reqWaiRequest $ \x y -> x { reqWaiRequest = y }

class HasHeaders env where
    headersL :: Lens' env [Header]

instance HasHeaders Request where
    headersL = lens requestHeaders $ \x y -> x { requestHeaders = y }

instance HasHeaders Response where
    headersL = lens responseHeaders $ \x y -> mapResponseHeaders (const y) x

headerL :: HeaderName -> Lens' [Header] (Maybe ByteString)
headerL h = lens getter setter
  where
    getter :: [Header] -> Maybe ByteString
    getter = lookup h

    setter :: [Header] -> Maybe ByteString -> [Header]
    setter hdrs = \case
        Nothing -> hdrs
        Just bs -> replaceBy (== h) bs hdrs

replaceBy :: (a -> Bool) -> b -> [(a, b)] -> [(a, b)]
replaceBy _ _ [] = []
replaceBy f b' (x@(a, _) : rest)
    | f a = (a, b') : replaceBy f b' rest
    | otherwise = x : replaceBy f b' rest

-- | Functions to trigger a Server Unavailable condition
--
-- The only (centralized) place you can handle application exceptions is in
-- 'yesodMiddleware'. But it requires you return a single @'ToTypedContent' res
-- => res@; you can't use 'selectRep'. You can use 'selectRep' in
-- 'errorHandler', but by that point the exception has become @'InternalError'
-- 'Text'@.
--
-- As a workaround, we use a 'yesodMiddleware' to catch an exception and pack
-- details into an 'InternalError'. We can then look for such cases in
-- 'errorHandler' and do what we want there.
--
-- This module encapsulates the pattern /specifically/ for a "Server
-- Unavailable" case. If we grow more cases, we might need to re-structure this.
--
module Restyled.ServerUnavailable
    ( serverUnavailable
    , serverUnavailableMessage
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Yesod
import Yesod.Core.Types (HandlerContents(HCError))

serverUnavailable :: MonadHandler m => Text -> m a
serverUnavailable = liftIO . throwIO . HCError . InternalError . (prefix <>)

serverUnavailableMessage :: ErrorResponse -> Maybe Text
serverUnavailableMessage = \case
    InternalError x -> T.stripPrefix prefix x
    _ -> Nothing

prefix :: Text
prefix = "XServerUnavailable:"

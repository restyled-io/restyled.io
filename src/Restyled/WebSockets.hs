module Restyled.WebSockets
    (
    -- * Helpers
    -- | TODO: I think a since-fixed bug was causing the need for this
      ignoringConnectionException

    -- * Wrappers
    , sendTextDataAck

    -- * Re-export
    , module Yesod.WebSockets
    ) where

import Restyled.Prelude

import qualified Network.WebSockets as WS
import Yesod.WebSockets

ignoringConnectionException :: MonadUnliftIO m => m () -> m ()
ignoringConnectionException = handle ignoreConnectionException

ignoreConnectionException :: Applicative f => WS.ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a >> receiveData

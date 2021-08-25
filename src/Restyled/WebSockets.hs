module Restyled.WebSockets
    (
    -- * Helpers
    -- | TODO: I think a since-fixed bug was causing the need for this
    --
    -- Try to write new code without it and see if it breaks. If not, remove it
    -- where it is used currently.
    --
      ignoringConnectionException

    -- * Wrappers
    , sendTextDataAck

    -- * Re-export
    , module Yesod.WebSockets
    ) where

import Restyled.Prelude

import qualified Network.WebSockets as WS
import Yesod.WebSockets

ignoringConnectionException
    :: MonadUnliftIO m => WebSocketsT m () -> WebSocketsT m ()
ignoringConnectionException = handle ignoreConnectionException

ignoreConnectionException :: Applicative f => WS.ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a >> receiveData

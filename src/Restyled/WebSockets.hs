module Restyled.WebSockets
    ( sendTextDataAck
    , module Yesod.WebSockets
    ) where

import Restyled.Prelude

import qualified Network.WebSockets as WS
import Yesod.WebSockets

sendTextDataAck :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a >> receiveData

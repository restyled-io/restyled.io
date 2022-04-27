module Restyled.WebSockets
    ( WebSocketsT
    , WebSocketsData
    , webSockets
    ) where

import Restyled.Prelude

import Network.WebSockets (WebSocketsData)
import qualified Network.WebSockets as WS
import Yesod.Core (MonadHandler)
import Yesod.WebSockets hiding (webSockets)
import qualified Yesod.WebSockets as WS

webSockets
    :: (MonadHandler m, MonadUnliftIO m, WebSocketsData a)
    => Int
    -- ^ Send a keep-alive if nothing has been sent in given number of seconds
    -> a
    -- ^ Keep-alive data to send
    -> ((a -> WebSocketsT m Text) -> WebSocketsT m b)
    -- ^ Function to call to send your data
    -> m ()
webSockets period element f = do
    now <- getCurrentTime
    ref <- newIORef now

    WS.webSockets $ ignoringConnectionException $ do
        race_
            (sendKeepAlive ref period element)
            (f $ sendTextDataAckTracked ref)

-- | 'sendTextDataAck' but stash the time sent in the given 'IORef'
--
-- For use with 'sendKeepAlive'.
--
sendTextDataAckTracked
    :: (MonadIO m, MonadReader WS.Connection m, WebSocketsData a)
    => IORef UTCTime
    -> a
    -> m Text
sendTextDataAckTracked ref x = do
    atomicWriteIORef ref =<< getCurrentTime
    sendTextDataAck x

-- | Send a keep-alive, if no messages have been sent in the given period
sendKeepAlive
    :: (MonadIO m, MonadReader WS.Connection m, WebSocketsData a)
    => IORef UTCTime
    -> Int
    -- ^ Send if nothing has been sent in given number of seconds
    -> a
    -- ^ Data to send
    -> m b
sendKeepAlive ref period element = forever $ do
    threadDelay $ 5 * 1000000
    sinceLastOutput <- diffUTCTime <$> getCurrentTime <*> readIORef ref
    when (sinceLastOutput >= fromIntegral period)
        $ void
        $ sendTextDataAckTracked ref element

ignoringConnectionException :: MonadUnliftIO m => m () -> m ()
ignoringConnectionException = handle ignoreConnectionException

ignoreConnectionException :: Applicative f => WS.ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck
    :: (MonadIO m, MonadReader WS.Connection m, WebSocketsData a) => a -> m Text
sendTextDataAck a = sendTextData a >> receiveData

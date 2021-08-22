module Restyled.WebSockets
    (
    -- * Async
      withAsyncWebSockets
    , withAsyncWebSockets_

    -- * Helpers
    -- | TODO: I think a since-fixed bug was causing the need for this
    --
    -- Try to write new code without it and see if it breaks. If not, remove it
    -- where it is used currently.
    --
    , ignoringConnectionException

    -- * Wrappers
    , sendTextDataAck
    , sendClose

    -- * Re-export
    , module Yesod.WebSockets
    ) where

import Restyled.Prelude

import qualified Network.WebSockets as WS
import Yesod.Core (MonadHandler)
import Yesod.WebSockets hiding (sendClose)

data Message a = Send a | Done

withAsyncWebSockets
    :: (MonadHandler m, MonadUnliftIO m, WS.WebSocketsData a)
    => ((a -> m ()) -> m b)
    -> m b
withAsyncWebSockets f = do
    c <- newChan
    a <- async $ do
        x <- f $ writeChan c . Send
        x <$ writeChan c Done

    webSockets $ loop c
    wait a
  where
    loop
        :: (MonadIO m, WS.WebSocketsData a)
        => Chan (Message a)
        -> ReaderT WS.Connection m ()
    loop c = do
        msg <- readChan c

        case msg of
            Send x -> do
                void $ sendTextDataAck x
                loop c
            Done -> sendClose @_ @Text ""

withAsyncWebSockets_
    :: (MonadHandler m, MonadUnliftIO m, WS.WebSocketsData a)
    => ((a -> m ()) -> m b)
    -> m ()
withAsyncWebSockets_ = void . withAsyncWebSockets

ignoringConnectionException
    :: MonadUnliftIO m => WebSocketsT m () -> WebSocketsT m ()
ignoringConnectionException = handle ignoreConnectionException

ignoreConnectionException :: Applicative f => WS.ConnectionException -> f ()
ignoreConnectionException _ = pure ()

sendTextDataAck :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m Text
sendTextDataAck a = sendTextData a >> receiveData

-- | <https://github.com/yesodweb/yesod/issues/1599>
sendClose
    :: (MonadIO m, WS.WebSocketsData a, MonadReader WS.Connection m)
    => a
    -> m ()
sendClose x = do
    conn <- ask
    liftIO $ WS.sendClose conn x

module RIO.Redis
    ( HasRedis(..)
    , runRedis
    , runRedisUntraced

    -- * Re-export
    , Redis
    , Connection
    , checkedConnect

    -- * Actions we use
    -- | FIXME: encapsulate our own push/pop?
    , llen
    , lpush
    , brpop

    -- * Convenience
    , encodeStrict
    ) where

import RIO

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Database.Redis (Connection, Redis, brpop, checkedConnect, llen, lpush)
import qualified Database.Redis as Redis
import Restyled.Tracing
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasRedis env where
    redisConnectionL :: Lens' env Connection

instance HasRedis Connection where
    redisConnectionL = id

instance HasRedis env => HasRedis (HandlerData child env) where
    redisConnectionL = envL . siteL . redisConnectionL

runRedis
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasRedis env
       , HasTracingApp env
       , HasTransactionId env
       )
    => Redis a
    -> m a
runRedis action = do
    conn <- view redisConnectionL
    traceAppSegment "runRedis" $ do
        liftIO $ Redis.runRedis conn action

runRedisUntraced
    :: (MonadIO m, MonadReader env m, HasRedis env) => Redis a -> m a
runRedisUntraced action = do
    conn <- view redisConnectionL
    liftIO $ Redis.runRedis conn action

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode

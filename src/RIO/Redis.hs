module RIO.Redis
    ( HasRedis(..)
    , runRedis

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
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasRedis env where
    redisConnectionL :: Lens' env Connection

instance HasRedis Connection where
    redisConnectionL = id

instance HasRedis env => HasRedis (HandlerData child env) where
    redisConnectionL = envL . siteL . redisConnectionL

runRedis :: (HasRedis env, MonadReader env m, MonadIO m) => Redis a -> m a
runRedis action = do
    conn <- view redisConnectionL
    liftIO $ Redis.runRedis conn action

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode

module RIO.Redis
    ( HasRedis(..)
    , runRedis

    -- * Re-export
    , Redis
    , Connection

    -- * Actions we use
    -- | FIXME: encapsulate our own push/pop?
    , lpush
    , brpop
    )
where

import RIO

import Database.Redis (Connection, Redis, brpop, lpush)
import qualified Database.Redis as Redis

class HasRedis env where
    redisConnectionL :: Lens' env Connection

runRedis :: HasRedis env => Redis a -> RIO env a
runRedis action = do
    conn <- view redisConnectionL
    liftIO $ Redis.runRedis conn action

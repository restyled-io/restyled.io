module Restyled.Cache
    ( MonadCache(..)
    , CacheKey
    , cacheKey
    , caching

    -- * For use in concrete Reader instances
    , getCacheRedis
    , setCacheRedis
    ) where

import Restyled.Prelude hiding (get, set)

import qualified Data.Text as T
import Database.Redis (expire, get, set)
import Restyled.Yesod

newtype CacheKey = CacheKey Text
    deriving stock Eq
    deriving newtype (Show, Hashable)

-- | Safe constructor for @'CacheKey'@ from a hierarchical path
cacheKey :: [Text] -> CacheKey
cacheKey = CacheKey . T.intercalate "."

class Monad m => MonadCache m where
    getCache :: FromJSON a => CacheKey -> m (Maybe a)
    setCache :: ToJSON a => CacheKey -> a -> m ()

    withCache :: (FromJSON a, ToJSON a) => CacheKey -> m a -> m a
    withCache key action = maybe produce pure =<< getCache key
      where
        produce = do
            val <- action
            val <$ setCache key val

instance MonadCache m => MonadCache (ExceptT e m) where
    getCache = lift . getCache
    setCache k = lift . setCache k

instance MonadCache m => MonadCache (SqlPersistT m) where
    getCache = lift . getCache
    setCache k = lift . setCache k

instance HasRedis env => MonadCache (RIO env) where
    getCache = getCacheRedis
    setCache = setCacheRedis

instance HasRedis env => MonadCache (HandlerFor env) where
    getCache = getCacheRedis
    setCache = setCacheRedis

-- | Cache a value-producing action at the given key
--
-- The key will expire according to the @'setCache'@ implementation regardless
-- of how often its accessed. I.e. values aren't re-set after access.
--
caching :: (ToJSON a, FromJSON a, MonadCache m) => [Text] -> m a -> m a
caching = withCache . cacheKey

-- | @'getCache'@ implementation for any @'MonadReader'@ over @'HasRedis' env@
--
-- NB. All errors are smashed into a @'Nothing'@ result.
--
getCacheRedis
    :: (MonadIO m, MonadReader env m, HasRedis env, FromJSON a)
    => CacheKey
    -> m (Maybe a)
getCacheRedis (CacheKey key) = runRedis $ do
    eVal <- get $ encodeUtf8 key
    pure $ decodeStrict =<< join (hush eVal)

-- | @'setCache'@ implementation for any @'MonadReader'@ over @'HasRedis' env@
--
-- Keys are set to expire in 300 seconds.
--
setCacheRedis
    :: (ToJSON a, HasRedis env, MonadReader env m, MonadIO m)
    => CacheKey
    -> a
    -> m ()
setCacheRedis (CacheKey key) obj = runRedis $ do
    void $ set (encodeUtf8 key) $ encodeStrict obj
    void $ expire (encodeUtf8 key) 300

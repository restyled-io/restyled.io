module Restyled.Cache
    ( MonadCache(..)
    , CacheKey(..)
    , withCache
    , caching
    )
where

import Restyled.Prelude hiding (get, set)

import qualified Data.Text as T
import Database.Redis (expire, get, set)
import Restyled.Yesod

newtype CacheKey = CacheKey Text

class Monad m => MonadCache m where
    getCache :: FromJSON a => CacheKey -> m (Maybe a)
    setCache :: ToJSON a => CacheKey -> a -> m ()

instance HasRedis env => MonadCache (HandlerFor env) where
    getCache (CacheKey key) = runRedis $ do
        eVal <- get $ encodeUtf8 key
        pure $ decodeStrict =<< join (hush eVal)

    setCache (CacheKey key) obj = runRedis $ do
        void $ set (encodeUtf8 key) $ encodeStrict obj
        void $ expire (encodeUtf8 key) 300

instance MonadCache m => MonadCache (ExceptT e m) where
    getCache = lift . getCache
    setCache k = lift . setCache k

instance MonadCache m => MonadCache (SqlPersistT m) where
    getCache = lift . getCache
    setCache k = lift . setCache k

-- | Cache a value-producing action at the given key
--
-- The key will expire according to the @'setCache'@ implementation regardless
-- of how often its accessed. I.e. values aren't re-set after access.
--
withCache :: (MonadCache m, FromJSON a, ToJSON a) => CacheKey -> m a -> m a
withCache key action = maybe produce pure =<< getCache key
  where
    produce = do
        val <- action
        val <$ setCache key val

-- | A version of @'withCache'@ that takes a /key path/ of sorts
--
-- > caching ["foo", "bar"] f
--
-- is equivalent to
--
-- > withCache (CacheKey "foo.bar") f
--
caching :: (ToJSON a, FromJSON a, MonadCache m) => [Text] -> m a -> m a
caching k = withCache (CacheKey $ T.intercalate "." k)

module Restyled.Cache
    ( MonadCache(..)
    , CacheKey
    , cacheKey
    , caching
    ) where

import Restyled.Prelude hiding (get, set)

import qualified Data.Text as T

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

-- | Cache a value-producing action at the given key
--
-- The key will expire according to the @'setCache'@ implementation regardless
-- of how often its accessed. I.e. values aren't re-set after access.
--
caching :: (ToJSON a, FromJSON a, MonadCache m) => [Text] -> m a -> m a
caching = withCache . cacheKey

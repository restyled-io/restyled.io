{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Restyled.Cache.Memory
    ( CacheMemory
    , newCacheMemory
    , HasCacheMemory(..)
    , getCacheMemory
    , setCacheMemory
    ) where

import Restyled.Prelude

import qualified Data.Aeson as Aeson
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.HashMap as HashMap
import Restyled.Cache
import Restyled.Yesod
import Yesod.Core.Types (HandlerData)

newtype CacheMemory = CacheMemory
    { unCacheMemory :: IORef (HashMap CacheKey ByteString)
    }

newCacheMemory :: MonadIO m => m CacheMemory
newCacheMemory = CacheMemory <$> newIORef HashMap.empty

class HasCacheMemory env where
    cacheMemoryL :: Lens' env CacheMemory

instance HasCacheMemory CacheMemory where
    cacheMemoryL = id

instance HasCacheMemory env => HasCacheMemory (HandlerData child env) where
    cacheMemoryL = envL . siteL . cacheMemoryL

instance HasCacheMemory env => MonadCache (HandlerFor env) where
    getCache = getCacheMemory
    setCache = setCacheMemory

data Cached a = Cached
    { cachedExpiresAt :: UTCTime
    , cachedValue :: a
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

getCacheMemory
    :: (MonadIO m, MonadReader env m, HasCacheMemory env, FromJSON a)
    => CacheKey
    -> m (Maybe a)
getCacheMemory k = do
    cm <- unCacheMemory <$> view cacheMemoryL
    hm <- readIORef cm
    now <- getCurrentTime

    let
        mcached = do
            bs <- HashMap.lookup k hm
            Aeson.decode $ BSL.fromStrict bs

    case mcached of
        Nothing -> pure Nothing
        Just Cached {..} | cachedExpiresAt <= now ->
            atomicModifyIORef' cm $ (, Nothing) . HashMap.delete k
        Just Cached {..} -> pure cachedValue

-- | @'setCache'@ implementation for any @'MonadReader'@ over @'HasRedis' env@
--
-- Keys are set to expire in 300 seconds.
--
setCacheMemory
    :: (MonadIO m, MonadReader env m, HasCacheMemory env, ToJSON a)
    => CacheKey
    -> a
    -> m ()
setCacheMemory k obj = do
    cm <- unCacheMemory <$> view cacheMemoryL
    now <- getCurrentTime
    atomicModifyIORef' cm $ (, ()) . HashMap.insert k (encoded now)
  where
    encoded now = BSL.toStrict $ Aeson.encode $ Cached
        { cachedExpiresAt = addUTCTime 300 now
        , cachedValue = obj
        }

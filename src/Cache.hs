{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cache
    ( MonadCache(..)
    , CacheKey(..)
    , withCache
    , caching
    ) where

import Prelude

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (SqlPersistT)

newtype CacheKey = CacheKey Text

class Monad m => MonadCache m where
    getCache :: FromJSON a => CacheKey -> m (Maybe a)
    setCache :: ToJSON a => CacheKey -> a -> m ()

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

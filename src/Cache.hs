module Cache
    ( MonadCache(..)
    , CacheKey(..)
    , withCache
    ) where

import Prelude

import Data.Aeson
import Data.Text (Text)

newtype CacheKey = CacheKey Text

class Monad m => MonadCache m where
    getCache :: FromJSON a => CacheKey -> m (Maybe a)
    setCache :: ToJSON a => CacheKey -> a -> m ()

-- | Cache a value-producing action at the given key
--
-- The key will expire according to the @'setCache'@ implementation regardless
-- of how often its access. I.e. values aren't re-set after access.
--
withCache :: (MonadCache m, FromJSON a, ToJSON a) => CacheKey -> m a -> m a
withCache key action = maybe produce pure =<< getCache key
  where
    produce = do
        val <- action
        val <$ setCache key val

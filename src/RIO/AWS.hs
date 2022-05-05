{-# OPTIONS_GHC -Wno-orphans #-}

module RIO.AWS
    ( HasAWS(..)
    , discoverAWS
    -- , implementAWS
    -- , MonadAWS(..)
    , send
    , paginate
    ) where

import RIO

import qualified Amazonka as AWS
import Conduit
-- import Control.Monad.Catch (MonadCatch(..))
-- import qualified Control.Monad.Trans.AWS as AWS
-- import Network.AWS (AWS, MonadAWS(..), paginate, runAWS, runResourceT, send)
import RIO.Orphans ()
-- import qualified UnliftIO.Exception as UnliftIO
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasAWS env where
    awsEnvL :: Lens' env AWS.Env

instance HasAWS AWS.Env where
    awsEnvL = id

instance HasAWS env => HasAWS (HandlerData child env) where
    awsEnvL = envL . siteL . awsEnvL

discoverAWS
    :: MonadIO m
    => Bool -- ^ Debug?
    -> m AWS.Env
discoverAWS _debug = do
    -- let level = if debug then AWS.Debug else AWS.Info
    -- lgr <- AWS.newLogger level stdout
    liftIO $ AWS.newEnv AWS.discover -- <&> AWS.envLogger .~ lgr

send
    :: (MonadResource m, MonadReader env m, HasAWS env, AWS.AWSRequest a)
    => a
    -> m (AWS.AWSResponse a)
send req = do
    env <- view awsEnvL
    AWS.send env req

paginate
    :: (MonadResource m, MonadReader env m, HasAWS env, AWS.AWSPager a)
    => a
    -> ConduitM () (AWS.AWSResponse a) m ()
paginate req = do
    env <- view awsEnvL
    AWS.paginateEither env req >>= hoistEither

hoistEither :: MonadIO m => Either AWS.Error a -> m a
hoistEither = either (liftIO . throwIO) pure

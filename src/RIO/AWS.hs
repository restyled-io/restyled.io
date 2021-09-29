{-# OPTIONS_GHC -Wno-orphans #-}

module RIO.AWS
    ( HasAWS(..)
    , discoverAWS
    , implementAWS
    , MonadAWS(..)
    , send
    , paginate
    ) where

import RIO

import Control.Monad.Catch (MonadCatch(..))
import qualified Control.Monad.Trans.AWS as AWS
import Network.AWS (AWS, MonadAWS(..), paginate, runAWS, runResourceT, send)
import RIO.Orphans ()
import qualified UnliftIO.Exception as UnliftIO
import Yesod.Core.Types (HandlerData, HandlerFor)
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
discoverAWS debug = do
    let level = if debug then AWS.Debug else AWS.Info
    lgr <- AWS.newLogger level stdout
    liftIO $ AWS.newEnv AWS.Discover <&> AWS.envLogger .~ lgr

implementAWS
    :: (MonadUnliftIO m, MonadReader env m, HasAWS env) => AWS a -> m a
implementAWS req = do
    env <- view awsEnvL
    runResourceT $ runAWS env req

instance HasAWS env => MonadAWS (RIO env) where
    liftAWS = implementAWS

instance MonadCatch (HandlerFor app) where
    catch = UnliftIO.catch

instance HasAWS app => MonadAWS (HandlerFor app) where
    liftAWS = implementAWS

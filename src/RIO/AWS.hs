module RIO.AWS
    ( HasAWS(..)
    , runAWS
    , discoverAWS
    ) where

import RIO

import Control.Monad.Trans.AWS (runResourceT)
import qualified Control.Monad.Trans.AWS as AWS
import Yesod.Core.Types (HandlerData)
import Yesod.Lens

class HasAWS env where
    awsEnvL :: Lens' env AWS.Env

instance HasAWS AWS.Env where
    awsEnvL = id

instance HasAWS env => HasAWS (HandlerData child env) where
    awsEnvL = handlerEnvL . siteL . awsEnvL

runAWS
    :: (MonadIO m, MonadReader env m, HasAWS env, AWS.AWSRequest a)
    => a
    -> m (AWS.Rs a)
runAWS req = do
    env <- view awsEnvL
    liftIO $ runResourceT $ AWS.runAWST env $ AWS.send req

discoverAWS
    :: MonadIO m
    => Bool -- ^ Debug?
    -> m AWS.Env
discoverAWS debug = do
    let level = if debug then AWS.Debug else AWS.Info
    lgr <- AWS.newLogger level stdout
    liftIO $ AWS.newEnv AWS.Discover <&> AWS.envLogger .~ lgr

module RIO.AWS
    ( HasAWS(..)
    , discoverAWS
    , implementAWS
    , MonadAWS(..)
    , send
    , paginate
    ) where

import RIO

import qualified Control.Monad.Trans.AWS as AWS
import Network.AWS (AWS, MonadAWS(..), paginate, runAWS, runResourceT, send)
import Yesod.Core.Types (HandlerData)
import Yesod.Lens

class HasAWS env where
    awsEnvL :: Lens' env AWS.Env

instance HasAWS AWS.Env where
    awsEnvL = id

instance HasAWS env => HasAWS (HandlerData child env) where
    awsEnvL = handlerEnvL . siteL . awsEnvL

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

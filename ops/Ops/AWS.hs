module Ops.AWS
    ( runAWS
    , awaitAWS
    ) where

import Control.Lens
import Network.AWS hiding (runAWS)
import Network.AWS.Waiter
import System.IO (stdout)
import qualified Network.AWS as AWS

runAWS :: AWSRequest a => a -> IO (Rs a)
runAWS x = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover
    runResourceT $ AWS.runAWS (env & envLogger .~ lgr) $ send x

awaitAWS :: AWSRequest a => Wait a -> a -> IO Accept
awaitAWS x y = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover
    runResourceT $ AWS.runAWS (env & envLogger .~ lgr) $ await x y

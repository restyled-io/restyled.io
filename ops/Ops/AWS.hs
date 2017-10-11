module Ops.AWS
    ( runAWS
    ) where

import Control.Lens
import Network.AWS hiding (runAWS)
import System.IO (stdout)
import qualified Network.AWS as AWS

runAWS :: AWSRequest a => a -> IO (Rs a)
runAWS x = do
    lgr <- newLogger Debug stdout
    env <- newEnv Discover
    runResourceT $ AWS.runAWS (env & envLogger .~ lgr) $ send x

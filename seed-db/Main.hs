module Main
    ( main
    ) where

import Restyled.Prelude

import Restyled.CLI
import Restyled.Development.Seeds

main :: IO ()
main = do
    setupCLI
    withApp $ \app -> do
        runRIO app $ runDB seedDB

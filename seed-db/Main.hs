module Main
    ( main
    ) where

import Restyled.Prelude

import Restyled.CLI
import Restyled.Development.Seeds

main :: IO ()
main = do
    setupCLI
    app <- loadApp
    runRIO app $ runDB seedDB

module Main
    ( main
    ) where

import Restyled.Prelude

import Restyled.CLI
import Restyled.SyncMarketplace

main :: IO ()
main = do
    setupCLI
    app <- loadApp
    runRIO app syncMarketplace

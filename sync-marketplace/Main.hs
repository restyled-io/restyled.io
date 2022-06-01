module Main
    ( main
    ) where

import Restyled.Prelude2

import Restyled.CLI
import Restyled.SyncMarketplace

main :: IO ()
main = do
    setupCLI
    runApp syncMarketplace

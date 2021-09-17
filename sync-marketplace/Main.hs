module Main
    ( main
    ) where

import Restyled.Prelude

import Restyled.SyncMarketplace

main :: IO ()
main = do
    setLineBuffering

    app <- loadApp
    runRIO app syncMarketplace

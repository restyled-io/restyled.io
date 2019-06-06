module Main (main) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Restyled.Application (runWaiApp)
import Restyled.Backend.Application (runBackend)
import Restyled.Backend.Foundation (loadBackend)
import Restyled.Backend.Marketplace (runSynchronize)
import Restyled.Development.Seeds (seedDB)
import Restyled.Foundation (loadApp)
import Restyled.Options
import Restyled.Settings (loadSettings)

main :: IO ()
main = do
    setLineBuffering
    RestyledOptions {..} <- parseRestyledOptions
    traverse_ loadEnvFrom oEnvFile
    backend <- loadBackend =<< loadSettings

    case oCommand of
        Web -> runWaiApp =<< loadApp backend
        Backend -> runRIO backend runBackend
        SyncMarketplace -> runRIO backend runSynchronize
        SeedDB -> runRIO backend $ runDB seedDB

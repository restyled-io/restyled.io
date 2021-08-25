module Main
    ( main
    ) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Restyled.Application (runWaiApp)
import Restyled.Backend.Foundation (loadBackend, loadBackendHandle)
import Restyled.Backend.MarketplaceSync (runSynchronize)
import Restyled.Development.Seeds (seedDB)
import Restyled.Export (runExport)
import Restyled.Foundation (loadApp)
import Restyled.Options
import Restyled.Settings (loadSettings)

main :: IO ()
main = do
    setLineBuffering
    RestyledOptions {..} <- parseRestyledOptions
    traverse_ loadEnvFrom oEnvFile
    settings <- loadSettings

    case oCommand of
        Web -> do
            backend <- loadBackend settings
            runWaiApp =<< loadApp backend
        Backend cmd -> do
            backend <- loadBackend settings
            runRIO backend $ case cmd of
                SyncMarketplace -> runSynchronize
                SeedDB -> runDB seedDB
        Export options -> do
            -- Log to stderr, since we produce actually useful output
            backend <- loadBackendHandle stderr settings
            runRIO backend $ runExport options

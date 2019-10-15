module Main (main) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Restyled.Application (runWaiApp)
import Restyled.Backend.Application
import Restyled.Backend.Foundation (loadBackend)
import Restyled.Backend.Marketplace (runSynchronize)
import Restyled.Backend.RestyleMachine (runRestyleMachinesCheck)
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
        Backend cmd -> runRIO backend $ case cmd of
            Webhooks -> runWebhooks
            Retries -> runRetries
            SyncMarketplace -> runSynchronize
            CheckRestyleMachines -> runRestyleMachinesCheck
            SeedDB -> runDB seedDB

module Main
    ( main
    ) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Restyled.Application (runWaiApp)
import Restyled.Backend.Foundation (loadBackend)
import Restyled.Foundation (loadApp)
import Restyled.Options
import Restyled.Settings (loadSettings)

main :: IO ()
main = do
    setLineBuffering
    RestyledOptions {..} <- parseRestyledOptions
    traverse_ loadEnvFrom oEnvFile
    settings <- loadSettings

    -- TODO: Collapse loadBackend/Backend into loadApp/App
    backend <- loadBackend settings
    runWaiApp =<< loadApp backend

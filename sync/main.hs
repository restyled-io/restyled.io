module Main (main) where

import Restyled.Prelude

import LoadEnv
import Restyled.Backend.Foundation
import Restyled.Backend.Marketplace (runSynchronize)
import Restyled.Settings

main :: IO ()
main = do
    setLineBuffering

    loadEnvFrom ".env.sync"
    backend <- loadBackend =<< loadSettings
    runRIO backend runSynchronize

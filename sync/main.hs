module Main (main) where

import Backend.Import

import Backend.Foundation
import Backend.Marketplace (runSynchronize)
import LoadEnv

main :: IO ()
main = do
    setLineBuffering

    loadEnvFrom ".env.sync"
    backend <- loadBackend =<< loadEnvSettings
    runRIO backend runSynchronize

module Main
    ( main
    ) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Restyled.Development.Seeds
import Restyled.Options

main :: IO ()
main = do
    setLineBuffering
    RestyledOptions {..} <- parseRestyledOptions
    traverse_ loadEnvFrom oEnvFile
    app <- loadApp
    runRIO app $ runDB seedDB

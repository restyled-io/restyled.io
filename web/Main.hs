module Main
  ( main
  ) where

import Restyled.Prelude

import Restyled.Application (runWaiApp)
import Restyled.CLI
import Restyled.Foundation (loadApp)

main :: IO ()
main = do
  setupCLI
  runWaiApp =<< loadApp

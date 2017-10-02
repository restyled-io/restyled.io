{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Restyler.Formatters
    ( runFormatters
    ) where

import ClassyPrelude

import System.Process (callProcess)
import System.FilePath (takeExtension)

data Formatter = Formatter
    { fExecutable :: String
    , fArguments :: [String]
    , fFilter :: FilePath -> Bool
    }

formatters :: [Formatter]
formatters =
    [ Formatter
        { fExecutable = "stylish-haskell"
        , fArguments = ["--inplace"]
        , fFilter = (`elem` [".hs", ".lhs"]) . takeExtension
        }
    ]

runFormatters :: [FilePath] -> IO ()
runFormatters paths = forM_ formatters $ \Formatter{..} ->
    callProcess fExecutable $ fArguments ++ filter fFilter paths

module Main (main) where

import Prelude

import Test.DocTest

main :: IO ()
main = doctest $ "-isrc" :
    -- doctest has been slow and breaks on some modules, so let's only target
    -- those that currently have doctest examples.
    [ "src/Helper"
    , "src/Network"
    ]

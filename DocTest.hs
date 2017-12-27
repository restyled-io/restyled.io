{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest $ "-isrc" :
    [ "src/Backend"
    , "src/GitHub"
    , "src/Handler"
    , "src/Helper"
    , "src/Network"
    ]

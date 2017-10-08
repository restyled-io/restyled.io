{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Ops.CloudFormation.Environment
import Ops.CloudFormation.Template
import Stratosphere
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    -- TODO: opt-parse, more optionality
    args <- getArgs

    L8.putStrLn $ encodeTemplate $ cfTemplate $ case args of
        ["beta"] -> defaultEnv
        ["staging"] -> stagingEnv
        ["prod"] -> prodEnv
        x -> error $ "Invalid args: " ++ show x

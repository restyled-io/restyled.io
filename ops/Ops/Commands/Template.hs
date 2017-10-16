module Ops.Commands.Template
    ( templateCommand
    ) where

import Ops.CloudFormation.Template
import Stratosphere (encodeTemplate)
import qualified Data.ByteString.Lazy.Char8 as L8

templateCommand :: IO ()
templateCommand = L8.putStrLn $ encodeTemplate cfTemplate

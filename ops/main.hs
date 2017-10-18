{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Semigroup ((<>))
import Ops.Commands.Deploy
import Ops.Commands.Template
import Options.Applicative
import Options.Generic

data Options
    = Template
    | Deploy DeployOptions

options :: Parser Options
options = subparser
    (  command "template" (pure Template
        `withInfo` "Output the Cloud Formation template")
    <> command "deploy" (Deploy <$> parseRecord
        `withInfo` "Deploy an image to an existing Stack")
    )

main :: IO ()
main = execParser (options `withInfo` "Operate Restyled.io") >>= \case
    Template -> templateCommand
    Deploy opts -> deployCommand opts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc

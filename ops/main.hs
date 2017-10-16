{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Semigroup ((<>))
import Ops.Commands.Stack
import Ops.Commands.Template
import Options.Applicative

data Options
    = Template
    | CreateStack StackOptions
    | UpdateStack StackOptions

options :: Parser Options
options = subparser
    (  command "template" (pure Template
        `withInfo` "Generate the Cloud Formation template to stdout")
    <> command "create-stack" (CreateStack <$> stackOptions
        `withInfo` "Create a Restyled Cloud Formation Stack")
    <> command "update-stack" (UpdateStack <$> stackOptions
        `withInfo` "Update a Restyled Cloud Formation Stack")
    )

main :: IO ()
main = execParser (options `withInfo` "Deploy and Operate Restyled") >>= \case
    Template -> templateCommand
    CreateStack opts -> createStack opts
    UpdateStack opts -> updateStack opts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc

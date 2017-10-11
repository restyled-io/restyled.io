{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.Monoid ((<>))
import Ops.Commands.Stack
import Ops.Commands.Template
import Options.Applicative

data Options
    = Template TemplateOptions
    | CreateStack TemplateOptions CreateParameters
    | UpdateStack StackUpdate

options :: Parser Options
options = subparser
    (  command "template" (Template <$> templateOptions
        `withInfo` "Generate the Cloud Formation template to stdout")
    <> command "create-stack" ((CreateStack <$> templateOptions <*> createParameterOptions)
        `withInfo` "Create a Restyled Cloud Formation Stack")
    <> command "update-stack-template" (UpdateStack <$> updateTemplateOptions
        `withInfo` "Update a Restyled Cloud Formation Stack")
    <> command "update-stack-parameters" (UpdateStack <$> updateParameterOptions
        `withInfo` "Update a Restyled Cloud Formation Stack parameters")
    )

main :: IO ()
main = execParser (options `withInfo` "Deploy and Operate Restyled") >>= \case
    Template opts -> templateCommand opts
    CreateStack topts popts -> createStack topts popts
    UpdateStack sopts -> updateStack sopts

withInfo :: Parser a -> String -> ParserInfo a
cmd `withInfo` desc = info (cmd <**> helper) $ fullDesc <> progDesc desc

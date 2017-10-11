{-# LANGUAGE OverloadedStrings #-}
module Ops.Commands.Template
    ( TemplateOptions(..)
    , templateOptions
    , templateCommand
    ) where

import Ops.CloudFormation.Environment
import Ops.CloudFormation.Template
import Options.Applicative
import Stratosphere (encodeTemplate)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T

data TemplateOptions
    = Staging
    | Prod
    | Custom Environment

customOptions :: Parser TemplateOptions
customOptions = Custom <$> envOptions
  where
    envOptions :: Parser Environment
    envOptions = Environment
        <$> (T.pack <$> strOption
            (  long "app"
            <> help "App name, e.g. Restyled"
            <> value (T.unpack $ envApp defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "name"
            <> help "Environment name, e.g. Beta"
            <> value (T.unpack $ envName defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "domain"
            <> help "Domain / hosted zone name, e.g. restyled.io"
            <> value (T.unpack $ envDomain defaultEnv)
            ))
        <*> optional (T.pack <$> strOption
            (  long "subdomain"
            <> help "Subdomain to deploy to, e.g. staging"
            ))
        <*> (T.pack <$> strOption
            (  long "certificate"
            <> help "ARN of Amazon Certificate, for the deployment domain"
            <> value (T.unpack $ envCertificateARN defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "ami"
            <> help "AMI for Apps cluster instances"
            <> value (T.unpack $ envAppsClusterAMI defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "cluster-size"
            <> help "Size to make Apps cluster"
            <> value (T.unpack $ envAppsClusterSize defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "instance-type"
            <> help "Instance type for Apps cluster"
            <> value (T.unpack $ envAppsClusterInstanceType defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "instance-role"
            <> help "Name of IAM role for cluster instances"
            <> value (T.unpack $ envAppsClusterInstanceRole defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "service-role"
            <> help "Name of IAM role for cluster services"
            <> value (T.unpack $ envAppsClusterInstanceRole defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "log-level"
            <> help "Log level for Apps services"
            <> value (T.unpack $ envAppsLogLevel defaultEnv)
            ))
        <*> (T.pack <$> strOption
            (  long "image-tag"
            <> help "Image tag for deployed Apps services"
            <> value (T.unpack $ envImageTag defaultEnv)
            ))
        <*> option auto
            (  long "app-count"
            <> help "Number of App services to run"
            <> value (envAppServiceCount defaultEnv)
            )
        <*> option auto
            (  long "backend-count"
            <> help "Number of Backend services to run"
            <> value (envBackendServiceCount defaultEnv)
            )

templateOptions :: Parser TemplateOptions
templateOptions = subparser
    (  command "staging" (toInfo $ pure Staging)
    <> command "prod" (toInfo $ pure Prod)
    <> command "custom" (toInfo customOptions)
    )

templateCommand :: TemplateOptions -> IO ()
templateCommand opts =
    L8.putStrLn $ encodeTemplate $ cfTemplate $ case opts of
        Staging -> stagingEnv
        Prod -> prodEnv
        Custom env -> env

toInfo :: Parser a -> ParserInfo a
toInfo cmd = info (cmd <**> helper) fullDesc

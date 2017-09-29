{-# LANGUAGE NoImplicitPrelude #-}
module Restyler.Options
    ( Options(..)
    , parseOptions
    ) where

import ClassyPrelude

import GitHub.Model
import Options.Applicative

data Options = Options
    { oGitHubAppId :: GitHubId
    , oGitHubAppKey :: Text
    , oInstallationId :: GitHubId
    , oRepoFullName :: RepoFullName
    , oPullRequestNumber :: PRNumber
    , oRestyledRoot :: Text
    }

options :: Parser Options
options = Options
    <$> (GitHubId <$> option auto
        (  long "github-app-id"
        <> metavar "ID"
        <> help "GitHub App Id"
        ))
    <*> (pack <$> strOption
        (  long "github-app-key"
        <> metavar "KEY"
        <> help "GitHub App Key"
        ))
    <*> (GitHubId <$> option auto
        (  long "installation-id"
        <> metavar "ID"
        <> help "Installation Id"
        ))
    <*> (RepoFullName . pack <$> strOption
        (  long "repository"
        <> metavar "OWNER/NAME"
        <> help "Repository"
        ))
    <*> (PRNumber <$> option auto
        (  long "pull-request"
        <> metavar "NUMBER"
        <> help "Pull Request"
        ))
    <*> (pack <$> strOption
        (  long "restyled-root"
        <> metavar "URL"
        <> help "Root for restyled.io"
        <> value "https://restyled.io"
        ))

parseOptions :: IO Options
parseOptions = execParser $ info (options <**> helper)
    (fullDesc <> progDesc "Restyle a GitHub Pull Request")

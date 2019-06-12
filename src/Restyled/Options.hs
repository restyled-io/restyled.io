module Restyled.Options
    ( RestyledOptions(..)
    , RestyledCommand(..)
    , parseRestyledOptions
    )
where

import Restyled.Prelude

import Options.Applicative

data RestyledOptions = RestyledOptions
    { oEnvFile :: Maybe FilePath
    , oCommand :: RestyledCommand
    }

data RestyledCommand
    = Web
    | Backend
    | SyncMarketplace
    | SeedDB

parseRestyledOptions :: IO RestyledOptions
parseRestyledOptions = execParser $ info (restyledOptions <**> helper) fullDesc

-- brittany-disable-next-binding

restyledOptions :: Parser RestyledOptions
restyledOptions = RestyledOptions
    <$> optional (strOption
        (  short 'e'
        <> long "env-file"
        <> metavar "PATH"
        <> help "Load PATH as a .env file"
        ))
    <*> subparser
        (  command "web" (info (pure Web) mempty)
        <> command "backend" (info (pure Backend) mempty)
        <> command "sync-marketplace" (info (pure SyncMarketplace) mempty)
        <> command "seed-db" (info (pure SeedDB) mempty)
        )

module Restyled.Options
    ( RestyledOptions(..)
    , RestyledCommand(..)
    , BackendCommand(..)
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
    | Backend BackendCommand

data BackendCommand
    = Webhooks
    | SyncMarketplace
    | Health
    | Reconcile
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
        <> command "webhooks" (info (pure $ Backend Webhooks) mempty)
        <> command "sync-marketplace" (info (pure $ Backend SyncMarketplace) mempty)
        <> command "health" (info (pure $ Backend Health) mempty)
        <> command "reconcile" (info (pure $ Backend Reconcile) mempty)
        <> command "seed-db" (info (pure $ Backend SeedDB) mempty)
        )

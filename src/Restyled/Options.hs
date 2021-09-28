module Restyled.Options
    ( RestyledOptions(..)
    , parseRestyledOptions
    ) where

import Restyled.Prelude

import Options.Applicative

newtype RestyledOptions = RestyledOptions
    { oEnvFile :: Maybe FilePath
    }

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

module Restyled.CLI
  ( setupCLI
  ) where

import Restyled.Prelude

import LoadEnv (loadEnvFrom)
import Options.Applicative

newtype RestyledOptions = RestyledOptions
  { oEnvFile :: Maybe FilePath
  }

setupCLI :: IO ()
setupCLI = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  RestyledOptions {..} <- parseRestyledOptions
  traverse_ loadEnvFrom oEnvFile

parseRestyledOptions :: IO RestyledOptions
parseRestyledOptions = execParser $ info (restyledOptions <**> helper) fullDesc

-- brittany-disable-next-binding

restyledOptions :: Parser RestyledOptions
restyledOptions =
  RestyledOptions
    <$> optional
      ( strOption
          ( short 'e'
              <> long "env-file"
              <> metavar "PATH"
              <> help "Load PATH as a .env file"
          )
      )

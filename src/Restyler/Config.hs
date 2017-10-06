{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Restyler.Config
    ( Config(..)
    , loadConfig
    , loadConfigFrom
    , Restyler(..)
    , restylePaths
    -- * Include Patterns
    , module Restyler.Config.Include
    -- * Exported for documentation only
    , configPath
    , defaultConfig
    , allRestylers
    ) where

import ClassyPrelude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Bifunctor (first)
import Data.Yaml
import Restyler.Config.Include
import System.Directory (doesFileExist)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V

data Config = Config
    { cEnabled :: Bool
    , cRestylers :: [Restyler]
    }
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cEnabled = True
    , cRestylers =
        [ namedRestyler' "stylish-haskell"
        , namedRestyler' "prettier"
        ]
    }
  where
    -- An unsafe version for use in this mostly static context
    namedRestyler' = either error id . namedRestyler

allRestylers :: [Restyler]
allRestylers =
    [ Restyler
        { rName = "stylish-haskell"
        , rCommand = "stylish-haskell"
        , rArguments = ["--inplace"]
        , rInclude = ["**/*.hs"]
        }
    , Restyler
        { rName = "prettier"
        , rCommand = "prettier"
        , rArguments = ["--write"]
        , rInclude = ["**/*.js", "**/*.jsx"]
        }
    , Restyler
        { rName = "hindent"
        , rCommand = "hindent-inplace"
        , rArguments = ["--"]
        , rInclude = ["**/*.hs"]
        }
    ]

namedRestyler :: MonadPlus m => Text -> m Restyler
namedRestyler name =
    case find ((== name) . pack . rName) allRestylers of
        Nothing -> fail $ unpack $ "Unknown restyler name: " <> name <> "."
        Just r -> pure r

instance FromJSON Config where
    parseJSON (Array v) = Config
        <$> pure (cEnabled defaultConfig)
        <*> mapM parseJSON (V.toList v)
    parseJSON (Object o) = Config
        -- Use default values if un-specified
        <$> o .:? "enabled" .!= cEnabled defaultConfig
        <*> o .:? "restylers" .!= cRestylers defaultConfig
    parseJSON v = typeMismatch "Config object or list of restylers" v

-- | Load from @'configPath'@ if it exists, otherwise '@defaultConfig'
loadConfig :: IO (Either String Config)
loadConfig = doesFileExist configPath >>= \e -> if e
    then loadConfigFrom configPath
    else pure $ Right defaultConfig

loadConfigFrom :: FilePath -> IO (Either String Config)
loadConfigFrom fp = first err <$> decodeFileEither fp
  where
    err = (("Invalid configuration " <> show fp <> ": ") <>)
        . prettyPrintParseException

configPath :: FilePath
configPath = ".restyled.yaml"

data Restyler = Restyler
    { rName :: String
    , rCommand :: String
    , rArguments :: [String]
    , rInclude :: [Include]
    }
    deriving (Eq, Show)

instance FromJSON Restyler where
    parseJSON v@(Object o) = case HM.toList o of
        [(k, v')] -> withObject "Override object"
            (\o' -> do
                Restyler{..} <- namedRestyler k
                Restyler -- Named + overrides
                    <$> pure (unpack k)
                    <*> o' .:? "command" .!= rCommand
                    <*> o' .:? "arguments" .!= rArguments
                    <*> o' .:? "include" .!= rInclude
            ) v'
        _ -> typeMismatch "Name with override object" v
    parseJSON (String t) = namedRestyler t
    parseJSON v = typeMismatch "Name or named with override object" v

restylePaths :: Restyler -> [FilePath] -> [FilePath]
restylePaths Restyler{..} = filter (includePath rInclude)

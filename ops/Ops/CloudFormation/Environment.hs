{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ops.CloudFormation.Environment
    ( Environment(..)
    , defaultEnv
    , stagingEnv
    , prodEnv
    , envDBName
    , envFQDN
    , envPrefix
    , envPrefixT
    , envTags
    , module Data.Monoid
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Stratosphere
import qualified Data.Text as T

data Environment = Environment
    { envApp :: Text
    , envName :: Text
    , envDomain :: Text
    , envSubdomain :: Maybe Text
    , envCertificateARN :: Text
    , envAppsClusterAMI :: Text
    , envAppsClusterSize :: Text
    , envAppsClusterInstanceType :: Text
    , envAppsClusterInstanceRole :: Text
    , envAppsServiceRole :: Text
    , envAppsLogLevel :: Text
    , envDBInstanceType :: Text
    , envDBInstanceSize :: Text -- GB
    , envCacheInstanceType :: Text
    , envCacheNodes :: Integer

    -- The above attributes get compiled into the template as they should rarely
    -- change between deployments of a given stack. The following attributes
    -- are here as defaults for Parameter values.

    , envImageTag :: Text
    , envAppServiceCount :: Int
    , envBackendServiceCount :: Int
    }

defaultEnv :: Environment
defaultEnv = Environment
    { envApp = "Restyled"
    , envName = "Beta"
    , envDomain = "restyled.io"
    , envSubdomain = Just "beta"
    , envCertificateARN = "arn:aws:acm:us-east-1:918255682310:certificate/ab8d47ed-d59e-461a-b54f-19febb31993a"
    , envAppsClusterAMI = "ami-ec33cc96"
    , envAppsClusterSize = "1"
    , envAppsClusterInstanceType = "t2.nano"
    , envAppsClusterInstanceRole = "ecsInstanceRole"
    , envAppsServiceRole = "ecsServiceRole"
    , envAppsLogLevel = "DEBUG"
    , envDBInstanceType = "db.t2.micro"
    , envDBInstanceSize = "5"
    , envCacheInstanceType = "cache.t2.micro"
    , envCacheNodes = 1
    , envImageTag = "latest"
    , envAppServiceCount = 1
    , envBackendServiceCount = 1
    }

stagingEnv :: Environment
stagingEnv = defaultEnv
    { envName = "Staging"
    , envSubdomain = Just "staging"
    }

prodEnv :: Environment
prodEnv = defaultEnv
    { envName = "Prod"
    , envSubdomain = Nothing
    , envAppsLogLevel = "INFO"
    }

envDBName :: Environment -> Text
envDBName = ("restyled_" <>) . T.toLower . envName

envFQDN :: Environment -> Text
envFQDN Environment{..} = maybe "" (<> ".") envSubdomain <> envDomain

-- | Prefix function to construct resource names
envPrefix :: Environment -> Val Text -> Val Text
envPrefix Environment{..} n = Join ""
    [ Literal envApp
    , Literal envName
    , n
    ]

-- | Same, but for when we can't use a @'Ref'@
envPrefixT :: Environment -> Text -> Text
envPrefixT Environment{..} = ((envApp <> envName) <>)

-- | Tags that should apply to all resources
envTags :: Environment -> [Tag]
envTags Environment{..} =
    [ tag "App" $ Literal envApp
    , tag "Environment" $ Literal envName
    ]

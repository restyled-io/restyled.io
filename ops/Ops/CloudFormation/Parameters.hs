{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Parameters
    ( cfParameters
    , fqdnRef
    , prefixRef
    , prefixJSON
    , defaultTags
    , toObject
    , toText
    ) where

import Data.Aeson (Value(..), Object, toJSON)
import Data.Text (Text)
import Stratosphere

cfParameters :: Parameters
cfParameters =
    [ parameter "App" "String" & default' ?~ "Restyled"
    , parameter "Environment" "String" & default' ?~ "Prod"
    , parameter "Domain" "String" & default' ?~ "restyled.io"
    , parameter "Subdomain" "String" & default' ?~ ""

    , parameter "DBInstanceType" "String" & default' ?~ "db.t2.micro"
    , parameter "DBStorageSize" "Number" & default' ?~ "5"
    , parameter "DBUsername" "String" & default' ?~ "root"
    , parameter "CacheInstanceType" "String" & default' ?~ "cache.t2.micro"
    , parameter "CacheClusterSize" "Number" & default' ?~ "1"

    , parameter "AppsClusterInstanceType" "String" & default' ?~ "t2.nano"
    , parameter "AppsClusterSize" "Number" & default' ?~ "1"
    , parameter "AppsImageName" "String" & default' ?~ "restyled/restyled"
    , parameter "AppsImageTag" "String" & default' ?~ "latest"
    , parameter "AppsAppServiceCount" "Number" & default' ?~ "1"
    , parameter "AppsBackendServiceCount" "Number" & default' ?~ "1"
    , parameter "AppsLogLevel" "String"
        & default' ?~ "INFO"
        & allowedValues ?~ ["DEBUG", "INFO", "WARN", "ERROR"]

    -- TODO: create these in-template
    , parameter "AppsClusterInstanceRole" "String" & default' ?~ "ecsInstanceRole"
    , parameter "AppsServiceRole" "String" & default' ?~ "ecsServiceRole"

    , parameter "CertificateARN" "String"
        & description ?~ "ARN of an Amazon SSL certificate for the Domain"
        & allowedPattern ?~ "^arn:aws:acm:.*"
        & constraintDescription ?~ "Must be a fully-qualified ARN value"
    , parameter "GitHubAppId" "Number"
        & description ?~ "Id for Restyled.io GitHub App"
    , parameter "GitHubAppKeyBase64" "String"
        & description ?~ "PEM key for Restyled.io GitHub App, base64-encoded"
    , parameter "DBPassword" "String"
        & description ?~ "Password for root user of the RDS instance"
        & minLength ?~ 10
        & noEcho ?~ Bool' True
    ]

-- | @{Subdomain}.{Domain}@ if @Subdomain@ is present, otherwise @{Domain}@
fqdnRef :: Val Text
fqdnRef = If "HasSubdomain"
    (Join "." [Ref "Subdomain", Ref "Domain"]) $ Ref "Domain"

-- | Prefix a reference with @{App}{Environment}@
prefixRef :: Val Text -> Val Text
prefixRef r = Join "" [Ref "App", Ref "Environment", r]

-- | Prefix a reference, then convert it to a JSON @'Value'@
prefixJSON :: Val Text -> Value
prefixJSON = toJSON . prefixRef

-- | The set of tags to apply to all resources that support it
defaultTags :: [Tag]
defaultTags =
    [ tag "App" $ Ref "App"
    , tag "Environment" $ Ref "Environment"
    ]

-- | De-construct an @'Object'@ @'Value'@ to just the @'Object'@
--
-- Many of the @"Stratosphere"@ functions that require JSON want an @'Object'@
-- specifically.
--
toObject :: Value -> Object
toObject (Object o) = o
toObject _ = error "Non-Object value"

-- | De-construct an @'String'@ @'Value'@ to just the @'Text'@
--
-- Useful for making @'Parameter'@ defaults display-friendly.
--
toText :: Value -> Text
toText (String t) = t
toText _ = error "Non-String value"

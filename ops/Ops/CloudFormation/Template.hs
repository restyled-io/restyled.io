{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Template
    ( cfTemplate
    ) where

import Data.Aeson (toJSON)
import Ops.CloudFormation.Environment
import Ops.CloudFormation.Resources.ALB
import Ops.CloudFormation.Resources.AppsCluster
import Ops.CloudFormation.Resources.AppsServices
import Ops.CloudFormation.Resources.Network
import Ops.CloudFormation.Resources.TaskDefinitions
import Stratosphere

cfTemplate :: Environment -> Template
cfTemplate env = template
    (  networkResources env
    <> albResources env
    <> appsClusterResources env
    <> appsServicesResources env
    <> taskDefinitionResources env
    )
    & parameters ?~
        [ parameter "ImageTag" "String"
            & default' ?~ toJSON (envImageTag env)
        , parameter "AppServiceCount" "Number"
            & default' ?~ toJSON (envAppServiceCount env)
        , parameter "BackendServiceCount" "Number"
            & default' ?~ toJSON (envBackendServiceCount env)

        -- Secrets need to be specified every time
        , parameter "GitHubAppId" "Number"
        , parameter "GitHubAppKeyBase64" "String"

        -- TODO: Setup RDS and ElastiCache in template
        , parameter "DatabaseURL" "String"
        , parameter "RedisURL" "String"
        ]
    & outputs ?~
        [ output "URL" $ Literal $ "https://" <> envFQDN env
        ]

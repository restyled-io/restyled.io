{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Ops.CloudFormation.Resources.AppsServices
    ( appsServicesResources
    ) where

import Data.Aeson.QQ (aesonQQ)
import Ops.CloudFormation.Parameters
import Stratosphere

appsServicesResources :: Resources
appsServicesResources =
    [ resource "AppsServiceRole"
        $ IAMRoleProperties
        $ iamRole (toObject [aesonQQ|
        {
            "Version": "2008-10-17",
            "Statement": [{
                "Effect":"Allow",
                "Principal":{"Service":["ecs.amazonaws.com"]},
                "Action":["sts:AssumeRole"]
            }]
        }
        |])
        & iamrRoleName ?~ prefixRef "ServiceRole"
        & iamrManagedPolicyArns ?~
            [ "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceRole"
            ]
    , resource "AppService"
        ( ECSServiceProperties
        $ ecsService (Ref "AppTaskDefinition")
        & ecssCluster ?~ Ref "AppsCluster"
        & ecssServiceName ?~ prefixRef "App"
        & ecssRole ?~ Ref "AppsServiceRole"
        & ecssDesiredCount ?~ Ref "AppsAppServiceCount"
        & ecssLoadBalancers ?~
            [ ecsServiceLoadBalancer (Literal 3000)
                & ecsslbContainerName ?~ prefixRef "App"
                & ecsslbTargetGroupArn ?~ Ref "ALBTargetGroup"
            ]
        )
        & dependsOn ?~ ["ALB", "Cache", "DB"]
    , resource "BackendService"
        ( ECSServiceProperties
        $ ecsService (Ref "BackendTaskDefinition")
        & ecssCluster ?~ Ref "AppsCluster"
        & ecssServiceName ?~ prefixRef "Backend"
        & ecssDesiredCount ?~ Ref "AppsBackendServiceCount"
        )
        & dependsOn ?~ ["Cache", "DB"]
    ]

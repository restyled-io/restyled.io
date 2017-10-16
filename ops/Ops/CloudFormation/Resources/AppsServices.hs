{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.AppsServices
    ( appsServicesResources
    ) where

import Ops.CloudFormation.Parameters
import Stratosphere

appsServicesResources :: Resources
appsServicesResources =
    [ resource "AppService"
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

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.AppsServices
    ( appsServicesResources
    ) where

import Ops.CloudFormation.Environment
import Stratosphere

-- References:
--
-- - Parameter ref: AppServiceCount
-- - Parameter ref: BackendServiceCount
--
appsServicesResources :: Environment -> Resources
appsServicesResources env =
    [ resource "AppService"
        ( ECSServiceProperties
        $ ecsService (Ref "AppTaskDefinition")
        & ecssCluster ?~ Ref "AppsCluster"
        & ecssServiceName ?~ envPrefix env "App"
        & ecssRole ?~ Literal (envAppsServiceRole env)
        & ecssDesiredCount ?~ Ref "AppServiceCount"
        & ecssLoadBalancers ?~
            [ ecsServiceLoadBalancer (Literal 3000)
                & ecsslbContainerName ?~ envPrefix env "App"
                & ecsslbTargetGroupArn ?~ Ref "ALBTargetGroup"
            ]
        )
        & dependsOn ?~ ["ALB"]
    , resource "BackendService"
        $ ECSServiceProperties
        $ ecsService (Ref "BackendTaskDefinition")
        & ecssCluster ?~ Ref "AppsCluster"
        & ecssServiceName ?~ envPrefix env "Backend"
        & ecssDesiredCount ?~ Ref "BackendServiceCount"
    ]

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.AppsCluster
    ( appsClusterResources
    ) where

import Ops.CloudFormation.Environment
import Stratosphere

appsClusterResources :: Environment -> Resources
appsClusterResources env =
    [ resource "AppsSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (envPrefix env "Apps")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (envPrefix env "Apps") : envTags env
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 31000
                & ecsgipToPort ?~ Literal 61000
                & ecsgipSourceSecurityGroupId ?~ Ref "ALBSecurityGroup"
            ]
    , resource "AppsLaunchConfiguration"
        $ AutoScalingLaunchConfigurationProperties
        $ autoScalingLaunchConfiguration
            (Literal $ envAppsClusterAMI env)
            (Literal $ envAppsClusterInstanceType env)
        & aslcIamInstanceProfile ?~ Literal (envAppsClusterInstanceRole env)
        & aslcSecurityGroups ?~ [Ref "AppsSecurityGroup"]
        & aslcBlockDeviceMappings ?~
            [ autoScalingLaunchConfigurationBlockDeviceMapping "/dev/xvdcz"
                & aslcbdmEbs ?~ (autoScalingLaunchConfigurationBlockDevice
                    & aslcbdVolumeSize ?~ Literal 22
                    & aslcbdVolumeType ?~ "gp2")
            ]
        & aslcUserData ?~ Base64 (Join "\n"
            [ "#!/bin/bash"
            , "# Join the apps cluser"
            , Join ""
                [ "echo ECS_CLUSTER="
                , envPrefix env "Apps"
                , " >> /etc/ecs/ecs.config"
                ]
            ])
    , resource "AppsAutoScalingGroup"
        ( AutoScalingAutoScalingGroupProperties
        $ autoScalingAutoScalingGroup "5" "1" -- [sic]
        & asasgLaunchConfigurationName ?~ Ref "AppsLaunchConfiguration"
        & asasgVPCZoneIdentifier ?~
            [ Ref "PrivateSubnet1"
            , Ref "PrivateSubnet2"
            , Ref "PrivateSubnet3"
            ]
        & asasgDesiredCapacity ?~ Literal (envAppsClusterSize env)
        & asasgTags ?~
            [ autoScalingAutoScalingGroupTagProperty
                "Name" (Literal True) (envPrefix env "Apps")
            , autoScalingAutoScalingGroupTagProperty
                "App" (Literal True) (Literal $ envApp env)
            , autoScalingAutoScalingGroupTagProperty
                "Env" (Literal True) (Literal $ envName env)
            ]
        )
        & resUpdatePolicy ?~ (updatePolicy
            & upAutoScalingRollingUpdate ?~ (autoScalingRollingUpdatePolicy
                & asrupMinInstancesInService ?~ Literal 1))
    , resource "AppsCluster"
        $ ECSClusterProperties
        $ ecsCluster
        & ecscClusterName ?~ envPrefix env "Apps"
    , resource "AppsClusterLogGroup"
        $ LogsLogGroupProperties
        $ logsLogGroup
        & llgLogGroupName ?~ envPrefix env "Apps"
    ]

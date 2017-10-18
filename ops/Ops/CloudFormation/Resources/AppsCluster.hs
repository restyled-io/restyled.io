{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.AppsCluster
    ( appsClusterResources
    ) where

import Ops.CloudFormation.Parameters
import Stratosphere

appsClusterResources :: Resources
appsClusterResources =
    [ resource "AppsSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (prefixRef "Apps")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (prefixRef "Apps") : defaultTags
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 31000
                & ecsgipToPort ?~ Literal 61000
                & ecsgipSourceSecurityGroupId ?~ Ref "ALBSecurityGroup"
            ]
    , resource "AppsLaunchConfiguration"
        $ AutoScalingLaunchConfigurationProperties
        $ autoScalingLaunchConfiguration
            (FindInMap "RegionAMIs" (Ref "AWS::Region") "Id")
            (Ref "AppsClusterInstanceType")
        & aslcIamInstanceProfile ?~ Ref "AppsClusterInstanceRole"
        & aslcSecurityGroups ?~ [Ref "AppsSecurityGroup"]
        & aslcUserData ?~ Base64 (Join "\n"
            [ "#!/bin/bash"
            , "# Join the apps cluser"
            , Join ""
                [ "echo ECS_CLUSTER="
                , prefixRef "Apps"
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
        & asasgDesiredCapacity ?~ Ref "AppsClusterSize"
        & asasgTags ?~
            [ autoScalingAutoScalingGroupTagProperty
                "Name" (Literal True) (prefixRef "Apps")
            , autoScalingAutoScalingGroupTagProperty
                "App" (Literal True) (Ref "App")
            , autoScalingAutoScalingGroupTagProperty
                "Environment" (Literal True) (Ref "Environment")
            ]
        )
        & resUpdatePolicy ?~ (updatePolicy
            & upAutoScalingRollingUpdate ?~ (autoScalingRollingUpdatePolicy
                & asrupMinInstancesInService ?~ Literal 1))
    , resource "AppsCluster"
        $ ECSClusterProperties
        $ ecsCluster
        & ecscClusterName ?~ prefixRef "Apps"
    , resource "AppsClusterLogGroup"
        $ LogsLogGroupProperties
        $ logsLogGroup
        & llgLogGroupName ?~ prefixRef "Apps"
    ]

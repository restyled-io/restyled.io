{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.ALB
    ( albResources
    ) where

import Ops.CloudFormation.Parameters
import Stratosphere

albResources :: Resources
albResources =
    [ resource "ALBSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (prefixRef "ALBs")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 80
                & ecsgipToPort ?~ Literal 80
                & ecsgipCidrIp ?~ "0.0.0.0/0"
            , ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 443
                & ecsgipToPort ?~ Literal 443
                & ecsgipCidrIp ?~ "0.0.0.0/0"
            ]
        & ecsgTags ?~ tag "Name" (prefixRef "ALBs") : defaultTags
    , resource "ALB"
        $ ElasticLoadBalancingV2LoadBalancerProperties
        $ elasticLoadBalancingV2LoadBalancer
        & elbvlbName ?~ prefixRef "ALB"
        & elbvlbScheme ?~ "internet-facing"
        & elbvlbSecurityGroups ?~ [Ref "ALBSecurityGroup"]
        & elbvlbSubnets ?~
            [ Ref "PublicSubnet1"
            , Ref "PublicSubnet2"
            , Ref "PublicSubnet3"
            ]
        & elbvlbTags ?~ tag "Name" (prefixRef "ALB") : defaultTags
    , resource "ALBHTTPListener"
        $ ElasticLoadBalancingV2ListenerProperties
        $ elasticLoadBalancingV2Listener
            [elasticLoadBalancingV2ListenerAction (Ref "ALBTargetGroup") "forward"]
            (Ref "ALB") (Literal 80) "HTTP"
    , resource "ALBHTTPSListener"
        $ ElasticLoadBalancingV2ListenerProperties
        $ elasticLoadBalancingV2Listener
            [elasticLoadBalancingV2ListenerAction (Ref "ALBTargetGroup") "forward"]
            (Ref "ALB") (Literal 443) "HTTPS"
        & elbvlCertificates ?~
            [ elasticLoadBalancingV2ListenerCertificate
                & elbvlcCertificateArn ?~ Ref "CertificateARN"
            ]
    , resource "ALBTargetGroup"
        $ ElasticLoadBalancingV2TargetGroupProperties
        $ elasticLoadBalancingV2TargetGroup (Literal 3000) "HTTP" (Ref "Vpc")
        & elbvtgHealthCheckPath ?~ "/revision"
        & elbvtgName ?~ prefixRef "ALBTargetGroup"
        & elbvtgTags ?~ tag "Name" (prefixRef "ALBTargetGroup") : defaultTags
    , resource "DNSRecord"
        $ Route53RecordSetProperties
        $ route53RecordSet (Join "" [fqdnRef, "."]) "A"
        -- N.B. Assumes a hosted zone named after the domain
        & rrsHostedZoneName ?~ Join "" [Ref "Domain", "."]
        & rrsAliasTarget ?~ route53RecordSetAliasTarget
            (GetAtt "ALB" "DNSName")
            (GetAtt "ALB" "CanonicalHostedZoneID")
    , resource "HealthCheck"
        ( Route53HealthCheckProperties
        $ route53HealthCheck
        ( route53HealthCheckHealthCheckConfig "HTTPS"
            & rhchccFullyQualifiedDomainName ?~ fqdnRef
            & rhchccResourcePath ?~ "/revision"
        )
        & rhcHealthCheckTags ?~
            [ route53HealthCheckHealthCheckTag "Name" $ prefixRef "Up"
            ]
        )
        & dependsOn ?~ ["AppService"]
    , resource "HealthCheckAlarm"
        $ CloudWatchAlarmProperties
        $ cloudWatchAlarm
            "LessThanThreshold"
            (Literal 1)         -- evaluate over 1 period
            "HealthCheckStatus"
            "AWS/Route53"
            (Literal 60)        -- check every 60s
            (Literal 1)         -- less than this many OK statuses
        & cwaAlarmName ?~ prefixRef "Up"
        & cwaAlarmDescription ?~ Join "" [fqdnRef, " is up"]
        & cwaDimensions ?~
            [ cloudWatchAlarmDimension "HealthCheckId" $ Ref "HealthCheck"
            ]
        & cwaStatistic ?~ "Minimum"
        & cwaAlarmActions ?~
            -- Stratosphere doesn't have Sub yet
            [ Join "" ["arn:aws:sns:", Ref "AWS::Region", ":", Ref "AWS::AccountId", ":NotifyMe"]
            ]
    ]

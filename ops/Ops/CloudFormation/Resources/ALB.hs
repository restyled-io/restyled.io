{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.ALB
    ( albResources
    ) where

import Ops.CloudFormation.Environment
import Stratosphere

albResources :: Environment -> Resources
albResources env =
    [ resource "ALBSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (envPrefix env "ALBs")
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
        & ecsgTags ?~ tag "Name" (envPrefix env "ALBs") : envTags env
    , resource "ALB"
        $ ElasticLoadBalancingV2LoadBalancerProperties
        $ elasticLoadBalancingV2LoadBalancer
        & elbvlbName ?~ envPrefix env "ALB"
        & elbvlbScheme ?~ "internet-facing"
        & elbvlbSecurityGroups ?~ [Ref "ALBSecurityGroup"]
        & elbvlbSubnets ?~
            [ Ref "PublicSubnet1"
            , Ref "PublicSubnet2"
            , Ref "PublicSubnet3"
            ]
        & elbvlbTags ?~ tag "Name" (envPrefix env "ALB") : envTags env
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
                & elbvlcCertificateArn ?~ Literal (envCertificateARN env)
            ]
    , resource "ALBTargetGroup"
        $ ElasticLoadBalancingV2TargetGroupProperties
        $ elasticLoadBalancingV2TargetGroup (Literal 3000) "HTTP" (Ref "Vpc")
        & elbvtgHealthCheckPath ?~ "/revision"
        & elbvtgName ?~ envPrefix env "ALBTargetGroup"
        & elbvtgTags ?~ tag "Name" (envPrefix env "ALBTargetGroup") : envTags env
    , resource "Subdomain"
        $ Route53RecordSetProperties
        $ route53RecordSet (Literal $ envFQDN env <> ".") "A"
        -- N.B. Assumes a hosted zone named after the domain
        & rrsHostedZoneName ?~ Literal (envDomain env <> ".")
        & rrsAliasTarget ?~ route53RecordSetAliasTarget
            (GetAtt "ALB" "DNSName")
            (GetAtt "ALB" "CanonicalHostedZoneID")
    ]

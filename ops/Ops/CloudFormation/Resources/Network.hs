{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.Network
    ( networkResources
    ) where

import Stratosphere
import Ops.CloudFormation.Environment

networkResources :: Environment -> Resources
networkResources env =
    [ resource "Vpc"
        $ EC2VPCProperties
        $ ec2VPC "10.0.0.0/16"
        & ecvpcEnableDnsSupport ?~ Literal True
        & ecvpcEnableDnsHostnames ?~ Literal True
        & ecvpcTags ?~ tag "Name" (envPrefix env "") : envTags env

    -- 3 Public Subnets across 3 AZs, routed through an IGW
    , resource "PublicSubnet1"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.0.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1a"
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (envPrefix env "Public1") : envTags env
    , resource "PublicSubnet2"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.1.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1b"
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (envPrefix env "Public2") : envTags env
    , resource "PublicSubnet3"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.2.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1c"
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (envPrefix env "Public2") : envTags env
    , resource "InternetGateway"
        $ EC2InternetGatewayProperties ec2InternetGateway
    , resource "AttachGateway"
        $ EC2VPCGatewayAttachmentProperties
        $ ec2VPCGatewayAttachment (Ref "Vpc")
        & ecvpcgaInternetGatewayId ?~ Ref "InternetGateway"
    , resource "RouteViaIgw"
        $ EC2RouteTableProperties
        $ ec2RouteTable (Ref "Vpc")
    , resource "PublicRouteViaIgw"
        $ EC2RouteProperties
        $ ec2Route (Ref "RouteViaIgw")
        & ecrDestinationCidrBlock ?~ "0.0.0.0/0"
        & ecrGatewayId ?~ Ref "InternetGateway"
    , resource "PublicSubnet1RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaIgw") (Ref "PublicSubnet1")
    , resource "PublicSubnet2RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaIgw") (Ref "PublicSubnet2")
    , resource "PublicSubnet3RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaIgw") (Ref "PublicSubnet3")

    -- 3 Private Subnets across 3 AZs, routed through a NAT in PublicSubnet1
    , resource "PrivateSubnet1"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.10.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1a"
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (envPrefix env "Private1") : envTags env
    , resource "PrivateSubnet2"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.11.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1b"
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (envPrefix env "Private2") : envTags env
    , resource "PrivateSubnet3"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.12.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ "us-east-1c"
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (envPrefix env "Private3") : envTags env
    , resource "NatEIP"
        $ EC2EIPProperties
        $ ec2EIP
        & eceipDomain ?~ "Vpc"
    , resource "Nat"
        $ EC2NatGatewayProperties
        $ ec2NatGateway (GetAtt "NatEIP" "AllocationId") (Ref "PublicSubnet1")
    , resource "RouteViaNat"
        $ EC2RouteTableProperties
        $ ec2RouteTable (Ref "Vpc")
    , resource "PrivateRouteViaNat"
        $ EC2RouteProperties
        $ ec2Route (Ref "RouteViaNat")
        & ecrDestinationCidrBlock ?~ "0.0.0.0/0"
        & ecrNatGatewayId ?~ Ref "Nat"
    , resource "PrivateSubnet1RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaNat") (Ref "PrivateSubnet1")
    , resource "PrivateSubnet2RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaNat") (Ref "PrivateSubnet2")
    , resource "PrivateSubnet3RouteTableAssociation"
        $ EC2SubnetRouteTableAssociationProperties
        $ ec2SubnetRouteTableAssociation (Ref "RouteViaNat") (Ref "PrivateSubnet3")
    ]

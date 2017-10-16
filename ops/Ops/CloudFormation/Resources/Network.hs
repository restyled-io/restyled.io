{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.Network
    ( networkResources
    ) where

import Ops.CloudFormation.Parameters
import Stratosphere

networkResources :: Resources
networkResources =
    [ resource "Vpc"
        $ EC2VPCProperties
        $ ec2VPC "10.0.0.0/16"
        & ecvpcEnableDnsSupport ?~ Literal True
        & ecvpcEnableDnsHostnames ?~ Literal True
        & ecvpcTags ?~ tag "Name" (prefixRef "") : defaultTags

    -- 3 Public Subnets across 3 AZs, routed through an IGW
    , resource "PublicSubnet1"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.0.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "a"]
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (prefixRef "Public1") : defaultTags
    , resource "PublicSubnet2"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.1.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "b"]
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (prefixRef "Public2") : defaultTags
    , resource "PublicSubnet3"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.2.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "c"]
        & ecsMapPublicIpOnLaunch ?~ Literal True
        & ecsTags ?~ tag "Name" (prefixRef "Public2") : defaultTags
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
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "a"]
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (prefixRef "Private1") : defaultTags
    , resource "PrivateSubnet2"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.11.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "b"]
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (prefixRef "Private2") : defaultTags
    , resource "PrivateSubnet3"
        $ EC2SubnetProperties
        $ ec2Subnet "10.0.12.0/24" (Ref "Vpc")
        & ecsAvailabilityZone ?~ Join "" [Ref "AWS::Region", "c"]
        & ecsMapPublicIpOnLaunch ?~ Literal False
        & ecsTags ?~ tag "Name" (prefixRef "Private3") : defaultTags
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

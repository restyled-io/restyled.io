{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.DataStores
    ( dataStoreResources
    ) where

import Ops.CloudFormation.Parameters
import Stratosphere

dataStoreResources :: Resources
dataStoreResources =
    [ resource "DBSubnetGroup"
        $ RDSDBSubnetGroupProperties
        $ rdsdbSubnetGroup "RDS subnets"
            [ Ref "PrivateSubnet1"
            , Ref "PrivateSubnet2"
            , Ref "PrivateSubnet3"
            ]
    , resource "DBSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (prefixRef "DBs")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (prefixRef "DBs") : defaultTags
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 5432
                & ecsgipToPort ?~ Literal 5432
                & ecsgipSourceSecurityGroupId ?~ Ref "AppsSecurityGroup"
            ]
    , resource "DB"
        ( RDSDBInstanceProperties
        $ rdsdbInstance (Ref "DBInstanceType")
        & rdsdbiAllocatedStorage ?~ Ref "DBStorageSize"
        & rdsdbiDBName ?~ "restyled"
        & rdsdbiEngine ?~ "postgres"
        & rdsdbiMasterUsername ?~ Ref "DBUsername"
        & rdsdbiMasterUserPassword ?~ Ref "DBPassword"
        & rdsdbiDBSubnetGroupName ?~ Ref "DBSubnetGroup"
        & rdsdbiVPCSecurityGroups ?~ [Ref "DBSecurityGroup"]
        )
        -- Apparently this dependency needs to be explicit
        & dependsOn ?~ ["DBSubnetGroup"]
    , resource "CacheSubnetGroup"
        $ ElastiCacheSubnetGroupProperties
        $ elastiCacheSubnetGroup "Cache subnets"
            [ Ref "PrivateSubnet1"
            , Ref "PrivateSubnet2"
            , Ref "PrivateSubnet3"
            ]
    , resource "CacheSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (prefixRef "Caches")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (prefixRef "Caches") : defaultTags
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 6379
                & ecsgipToPort ?~ Literal 6379
                & ecsgipSourceSecurityGroupId ?~ Ref "AppsSecurityGroup"
            ]
    , resource "Cache"
        ( ElastiCacheCacheClusterProperties
        $ elastiCacheCacheCluster
            (Ref "CacheInstanceType") "redis"
            (Ref "CacheClusterSize")
        & ecccCacheSubnetGroupName ?~ Ref "CacheSubnetGroup"
        & ecccVpcSecurityGroupIds ?~ [Ref "CacheSecurityGroup"]
        & ecccTags ?~ tag "Name" (prefixRef "Cache") : defaultTags
        )
        -- Apparently this dependency needs to be explicit
        & dependsOn ?~ ["CacheSubnetGroup"]
    ]

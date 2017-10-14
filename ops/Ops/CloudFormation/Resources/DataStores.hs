{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.DataStores
    ( dataStoreResources
    ) where

import Ops.CloudFormation.Environment
import Stratosphere

-- References:
--
-- - Parameter ref: DBUsername
-- - Parameter ref: DBPassword
--
dataStoreResources :: Environment -> Resources
dataStoreResources env =
    [ resource "DBSubnetGroup"
        $ RDSDBSubnetGroupProperties
        $ rdsdbSubnetGroup "RDS subnets"
            [ Ref "PrivateSubnet1"
            , Ref "PrivateSubnet2"
            , Ref "PrivateSubnet3"
            ]
    , resource "DBSecurityGroup"
        $ EC2SecurityGroupProperties
        $ ec2SecurityGroup (envPrefix env "DBs")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (envPrefix env "DBs") : envTags env
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 5432
                & ecsgipToPort ?~ Literal 5432
                & ecsgipSourceSecurityGroupId ?~ Ref "AppsSecurityGroup"
            ]
    , resource "DB"
        ( RDSDBInstanceProperties
        $ rdsdbInstance (Literal $ envDBInstanceType env)
        & rdsdbiAllocatedStorage ?~ Literal (envDBInstanceSize env)
        & rdsdbiDBName ?~ Literal (envDBName env)
        & rdsdbiEngine ?~ "postgres"
        & rdsdbiMasterUserPassword ?~ Ref "DBPassword"
        & rdsdbiMasterUsername ?~ Ref "DBUsername"
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
        $ ec2SecurityGroup (envPrefix env "Caches")
        & ecsgVpcId ?~ Ref "Vpc"
        & ecsgTags ?~ tag "Name" (envPrefix env "Caches") : envTags env
        & ecsgSecurityGroupIngress ?~
            [ ec2SecurityGroupIngressProperty "tcp"
                & ecsgipFromPort ?~ Literal 6379
                & ecsgipToPort ?~ Literal 6379
                & ecsgipSourceSecurityGroupId ?~ Ref "AppsSecurityGroup"
            ]
    , resource "Cache"
        ( ElastiCacheCacheClusterProperties
        $ elastiCacheCacheCluster
            (Literal $ envCacheInstanceType env) "redis"
            (Literal $ envCacheNodes env)
        & ecccCacheSubnetGroupName ?~ Ref "CacheSubnetGroup"
        & ecccVpcSecurityGroupIds ?~ [Ref "CacheSecurityGroup"]
        & ecccTags ?~ tag "Name" (envPrefix env "Cache") : envTags env
        )
        -- Apparently this dependency needs to be explicit
        & dependsOn ?~ ["CacheSubnetGroup"]
    ]

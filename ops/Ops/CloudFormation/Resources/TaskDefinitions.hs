{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Ops.CloudFormation.Resources.TaskDefinitions
    ( taskDefinitionResources
    ) where

import Data.Aeson (Value(..))
import Ops.CloudFormation.Environment
import Stratosphere

-- References:
--
-- - Parameter ref: ImageTag
-- - Parameter ref: DatabaseURL
-- - Parameter ref: RedisURL
-- - Parameter ref: GitHubAppId - Parameter ref: GitHubAppKey
--
taskDefinitionResources :: Environment -> Resources
taskDefinitionResources env =
    [ resource "AppTaskDefinition"
        ( ECSTaskDefinitionProperties
        $ ecsTaskDefinition
        & ecstdFamily ?~ envPrefix env "App"
        & ecstdContainerDefinitions ?~
            [ ecsTaskDefinitionContainerDefinition
                (Join ":" ["restyled/restyled", Ref "ImageTag"])
                (envPrefix env "App")
                & ecstdcdCommand ?~ ["/app/restyled.io"]
                & ecstdcdEnvironment ?~
                    [ ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "APPROOT"
                        & ecstdkvpValue ?~ ""
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "DATABASE_URL"
                        & ecstdkvpValue ?~ Ref "DatabaseURL"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "REDIS_URL"
                        & ecstdkvpValue ?~ Ref "RedisURL"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_KEY_BASE64"
                        & ecstdkvpValue ?~ Ref "GitHubAppKeyBase64"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "LOG_LEVEL"
                        & ecstdkvpValue ?~ Literal (envAppsLogLevel env)
                    ]
                & ecstdcdMemoryReservation ?~ Literal 128       -- Soft
                & ecstdcdMemory ?~ Literal 256                  -- Hard
                & ecstdcdPortMappings ?~
                    [ ecsTaskDefinitionPortMapping
                        & ecstdpmContainerPort ?~ Literal 3000
                        & ecstdpmHostPort ?~ Literal 0
                    ]
                & ecstdcdLogConfiguration ?~ (ecsTaskDefinitionLogConfiguration "awslogs"
                    & ecstdlcOptions ?~
                        [ ("awslogs-group", String $ envPrefixT env "Apps")
                        , ("awslogs-region", "us-east-1")
                        , ("awslogs-stream-prefix", String "App")
                        ])
            ]
        )
        & dependsOn ?~ ["AppsClusterLogGroup"]

    , resource "BackendTaskDefinition"
        ( ECSTaskDefinitionProperties
        $ ecsTaskDefinition
        & ecstdFamily ?~ envPrefix env "Backend"
        & ecstdVolumes ?~
            [ ecsTaskDefinitionVolume
                & ecstdvName ?~ "tmp"
                & ecstdvHost ?~ (ecsTaskDefinitionHostVolumeProperties
                    & ecstdhvpSourcePath ?~ "/tmp")
            , ecsTaskDefinitionVolume
                & ecstdvName ?~ "docker-socket"
                & ecstdvHost ?~ (ecsTaskDefinitionHostVolumeProperties
                    & ecstdhvpSourcePath ?~ "/var/run/docker.sock")
            ]
        & ecstdContainerDefinitions ?~
            [ ecsTaskDefinitionContainerDefinition
                (Join ":" ["restyled/restyled", Ref "ImageTag"])
                (envPrefix env "Backend")
                & ecstdcdUser ?~ "root" -- access to Docker deamon
                & ecstdcdCommand ?~ ["/app/restyled.io-backend"]
                & ecstdcdEnvironment ?~
                    [ ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "APPROOT"
                        & ecstdkvpValue ?~ Literal ("https://" <> envFQDN env)
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "DATABASE_URL"
                        & ecstdkvpValue ?~ Ref "DatabaseURL"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "REDIS_URL"
                        & ecstdkvpValue ?~ Ref "RedisURL"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_ID"
                        & ecstdkvpValue ?~ Ref "GitHubAppId"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "GITHUB_APP_KEY_BASE64"
                        & ecstdkvpValue ?~ Ref "GitHubAppKeyBase64"
                    , ecsTaskDefinitionKeyValuePair
                        & ecstdkvpName ?~ "LOG_LEVEL"
                        & ecstdkvpValue ?~ Literal (envAppsLogLevel env)
                    ]
                & ecstdcdMemoryReservation ?~ Literal 128       -- Soft
                & ecstdcdMemory ?~ Literal 256                  -- Hard
                & ecstdcdMountPoints ?~
                    [ ecsTaskDefinitionMountPoint
                        & ecstdmpSourceVolume ?~ "tmp"
                        & ecstdmpContainerPath ?~ "/tmp"
                    , ecsTaskDefinitionMountPoint
                        & ecstdmpSourceVolume ?~ "docker-socket"
                        & ecstdmpContainerPath ?~ "/var/run/docker.sock"
                    ]
                & ecstdcdLogConfiguration ?~ (ecsTaskDefinitionLogConfiguration "awslogs"
                    & ecstdlcOptions ?~
                        [ ("awslogs-group", String $ envPrefixT env "Apps")
                        , ("awslogs-region", "us-east-1")
                        , ("awslogs-stream-prefix", String "Backend")
                        ])
            ]
        )
        & dependsOn ?~ ["AppsClusterLogGroup"]
    ]

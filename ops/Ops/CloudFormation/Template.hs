{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Ops.CloudFormation.Template
    ( cfTemplate
    ) where

import Data.Aeson.QQ (aesonQQ)
import Data.Monoid ((<>))
import Ops.CloudFormation.Parameters
import Ops.CloudFormation.Resources.ALB
import Ops.CloudFormation.Resources.AppsCluster
import Ops.CloudFormation.Resources.AppsServices
import Ops.CloudFormation.Resources.DataStores
import Ops.CloudFormation.Resources.Network
import Ops.CloudFormation.Resources.TaskDefinitions
import Stratosphere
import qualified Data.HashMap.Lazy as HM

cfTemplate :: Template
cfTemplate = template
    (  networkResources
    <> dataStoreResources
    <> albResources
    <> appsClusterResources
    <> appsServicesResources
    <> taskDefinitionResources
    )
    & parameters ?~ cfParameters
    & mappings ?~ HM.fromList
        [("RegionAMIs", HM.fromList
            [("us-east-1", toObject [aesonQQ|{"Id": "ami-ec33cc96"}|])
            ])
        ]
    & conditions ?~ toObject [aesonQQ|
        {
            "HasSubdomain": {
                "Fn::Not": [{"Fn::Equals": [{"Ref": "Subdomain"}, ""]}]
            }
        }
        |]
    & outputs ?~
        [ output "URL" $ Join "" ["https://", fqdnRef]
        ]

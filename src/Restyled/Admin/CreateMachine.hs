module Restyled.Admin.CreateMachine
    ( createMachineForm
    , createMachineGetRestyleMachine
    ) where

import Restyled.Prelude

import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as BSL
import Restyled.Backend.RestyleMachine
import Restyled.Foundation
import Restyled.Models
import Restyled.Yesod

data CreateMachine = CreateMachine
    { name :: Text
    , host :: Text
    , certificateAuthority :: Text
    , certificate :: Text
    , privateKey :: Text
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

createMachineForm :: Form (Handler RestyleMachine)
createMachineForm = renderDivs $ createMachineGetRestyleMachine <$> areq
    (jsonField encodeCreateMachine)
    ("Machine JSON" { fsAttrs = [("rows", "30")] })
    (Just createMachineExample)

encodeCreateMachine :: CreateMachine -> Text
encodeCreateMachine = decodeUtf8 . BSL.toStrict . encodePretty' c
  where
    c = defConfig
        { confCompare =
            keyOrder
                [ "name"
                , "host"
                , "certificateAuthority"
                , "certificate"
                , "privateKey"
                ]
        }

createMachineExample :: CreateMachine
createMachineExample = CreateMachine
    { name = "..."
    , host = "..."
    , certificateAuthority = ""
    , certificate = ""
    , privateKey = ""
    }

createMachineGetRestyleMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => CreateMachine
    -> m RestyleMachine
createMachineGetRestyleMachine CreateMachine {..} = do
    (ec, _stderr, _stdout) <- withRestyleMachineEnv machine
        $ proc "docker" ["info"] readProcess
    pure machine { restyleMachineEnabled = ec == ExitSuccess }
  where
    machine = RestyleMachine
        { restyleMachineName = name
        , restyleMachineEnabled = False
        , restyleMachineHost = host
        , restyleMachineCaCert = certificateAuthority
        , restyleMachineCert = certificate
        , restyleMachineKey = privateKey
        , restyleMachineJobCount = 0
        , restyleMachineReconciling = False
        }

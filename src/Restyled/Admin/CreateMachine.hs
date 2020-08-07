{-# LANGUAGE DeriveAnyClass #-}

module Restyled.Admin.CreateMachine
    ( createMachinePlaceholder
    , createMachineGetRestyleMachine
    )
where

import Restyled.Prelude

import Restyled.Backend.RestyleMachine
import Restyled.Models

data CreateMachine = CreateMachine
    { name :: Text
    , host :: Text
    , certificateAuthority :: Text
    , certificate :: Text
    , privateKey :: Text
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

createMachinePlaceholder :: Text
createMachinePlaceholder = mconcat
    [ "{"
    , "\n  \"name\": \"...\","
    , "\n  \"host\": \"...\","
    , "\n  \"certificateAuthority\": \"...\","
    , "\n  \"certificateAuthority\": \"...\","
    , "\n  \"privateKey\": \"...\","
    , "\n}"
    ]

createMachineGetRestyleMachine
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => CreateMachine
    -> m RestyleMachine
createMachineGetRestyleMachine CreateMachine {..} = do
    ec <- withRestyleMachineEnv machine $ proc "docker" ["info"] runProcess
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

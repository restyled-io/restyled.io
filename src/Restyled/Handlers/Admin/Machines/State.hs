{-# LANGUAGE DeriveAnyClass #-}

module Restyled.Handlers.Admin.Machines.State
    ( getAdminMachinesStateR
    )
where

import Restyled.Prelude

import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Restyled.Backend.RestyleMachine
import qualified Restyled.Backend.Webhook as Webhook
import Restyled.Foundation
import Restyled.Models

data MachinesState = MachinesState
    { queueDepth :: Maybe Integer
    , jobCounts :: HashMap Text Int
    , containerProcesses :: HashMap Text Text
    }
    deriving stock Generic
    deriving anyclass ToJSON

getAdminMachinesStateR :: Handler Value
getAdminMachinesStateR = do
    machines <- runDB $ entityVal <$$> selectList
        [RestyleMachineEnabled ==. True]
        [Asc RestyleMachineName]
    machinesState <-
        MachinesState
        <$> runRedis Webhook.queueDepth
        <*> pure (getJobCounts machines)
        <*> getContainerProcesses machines
    pure $ toJSON machinesState

getJobCounts :: [RestyleMachine] -> HashMap Text Int
getJobCounts =
    HashMap.fromList . map (restyleMachineName &&& restyleMachineJobCount)

getContainerProcesses
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => [RestyleMachine]
    -> m (HashMap Text Text)
getContainerProcesses machines =
    fmap HashMap.fromList $ for machines $ \machine -> do
        -- TODO: do I care about errors here?
        (_ec, out, _stderr) <- withRestyleMachineEnv machine $ proc
            "docker"
            [ "ps"
            , "--format"
            , "{{.RunningFor}} {{.Image}}\\n  {{.Command}}"
            , "--no-trunc"
            ]
            readProcess
        pure (restyleMachineName machine, decodeUtf8 $ LBS.toStrict out)

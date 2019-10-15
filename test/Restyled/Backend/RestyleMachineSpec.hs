module Restyled.Backend.RestyleMachineSpec
    ( spec
    )
where

import Restyled.Test

import qualified Database.Persist as P
import Database.Persist.Sql (SqlPersistT)
import Restyled.Backend.RestyleMachine

spec :: Spec
spec = do
    withApp $ do
        describe "checkRestyleMachines" $ do
            it "disables machines failing the check" $ runDB $ do
                machineId1 <- insertMachine True
                machineId2 <- insertMachine True
                machineId3 <- insertMachine True
                Just machine2Before <- P.get machineId1

                result <-
                    checkRestyleMachines $ pure . (/= machineId2) . entityKey

                result `shouldBe` Disabled [Entity machineId2 machine2Before]
                fetchRestyleMachineEnabled machineId1 `shouldReturn` True
                fetchRestyleMachineEnabled machineId2 `shouldReturn` False
                fetchRestyleMachineEnabled machineId3 `shouldReturn` True

            it "won't cause zero enabled machines" $ runDB $ do
                machineId1 <- insertMachine True
                machineId2 <- insertMachine True
                machineId3 <- insertMachine True

                result <- checkRestyleMachines $ pure . const False

                result `shouldBe` NoneHealthy
                fetchRestyleMachineEnabled machineId1 `shouldReturn` True
                fetchRestyleMachineEnabled machineId2 `shouldReturn` True
                fetchRestyleMachineEnabled machineId3 `shouldReturn` True

    describe "withExtraEnvVars" $ do
        it "appends extra ENV vars to the Process Context" $ example $ do
            output <-
                withProcessContextNoLogging
                $ withExtraEnvVars [("FOO", "1")]
                $ proc "sh" ["-c", "echo $FOO"] readProcessStdout_

            output `shouldBe` "1\n"

        it "overrides ENV vars that already exist" $ example $ do
            output <-
                withProcessContextNoLogging
                $ withExtraEnvVars [("FOO", "1")]
                $ withExtraEnvVars [("FOO", "2")]
                $ proc "sh" ["-c", "echo $FOO"] readProcessStdout_

            output `shouldBe` "2\n"

insertMachine
    :: MonadIO m
    => Bool -- ^ Enabled
    -> SqlPersistT m RestyleMachineId
insertMachine enabled = insert RestyleMachine
    { restyleMachineName = "machine-x"
    , restyleMachineEnabled = enabled
    , restyleMachineHost = "tcp://1.1.1.1:1234"
    , restyleMachineCaCert = ""
    , restyleMachineCert = ""
    , restyleMachineKey = ""
    }

fetchRestyleMachineEnabled
    :: (MonadIO m, MonadFail m) => RestyleMachineId -> SqlPersistT m Bool
fetchRestyleMachineEnabled machineId = do
    Just machine <- P.get machineId
    pure $ restyleMachineEnabled machine

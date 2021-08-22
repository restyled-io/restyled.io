module Restyled.Backend.RestyleMachineSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Backend.RestyleMachine

spec :: Spec
spec = do
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

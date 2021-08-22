{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module RIO.Process.FollowSpec
    ( spec
    ) where

import RIO

import RIO.Process
import RIO.Process.Follow
import RIO.Time
import Test.Hspec

spec :: Spec
spec = do
    describe "followProcess" $ do
        it "captures output in the order it was generated" $ do
            ref <- newIORef ([] :: [(String, String)])

            let capture stream content = atomicModifyIORef' ref
                    $ \x -> (x <> [(stream, content)], ())
                script = "echo x; sleep 0.1; echo y >&2; sleep 0.1; echo z"

            void
                $ withProcessContextNoLogging
                $ proc "sh" ["-c", script]
                $ followProcess (capture "stdout") (capture "stderr")

            captured <- readIORef ref
            captured
                `shouldBe` [("stdout", "x"), ("stderr", "y"), ("stdout", "z")]

        it "captures output as it is generated" $ do
            ref <- newIORef ([] :: [(UTCTime, String)])

            let capture content = do
                    now <- getCurrentTime
                    atomicModifyIORef' ref $ \x -> (x <> [(now, content)], ())
                script = "echo x; sleep 0.1; echo y"

            void
                $ withProcessContextNoLogging
                $ proc "sh" ["-c", script]
                $ followProcess capture capture

            [t1, t2] <- map fst <$> readIORef ref
            diffUTCTime t2 t1 `shouldSatisfy` (> 0.1)

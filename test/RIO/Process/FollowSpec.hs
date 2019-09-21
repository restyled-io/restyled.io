module RIO.Process.FollowSpec
    ( spec
    )
where

import RIO

import RIO.Process
import RIO.Process.Follow
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


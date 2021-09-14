module Restyled.QueuesSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Queues

spec :: Spec
spec = withApp $ do
    describe "enqueue" $ do
        it "works with a single queue" $ do
            qs <- expectRight "Queues" $ readQueues "restyled:test"

            result <- runRedis $ do
                enqueue qs "{}"
                brpop ["restyled:test"] 3

            result `shouldBe` Right (Just ("restyled:test", "{}"))

        it "works with multiple frequencies" $ do
            qs <- expectRight "Queues"
                $ readQueues "restyled:testa/1, restyled:testb/2"

            results <- fmap (catMaybes . rights) $ runRedis $ do
                replicateM_ 10 $ enqueue qs "{}"
                replicateM 10 $ brpop ["restyled:testa", "restyled:testb"] 3

            let (a, b) = partition ((== "restyled:testa") . fst) results
            length a + length b `shouldBe` 10
            length a `shouldBeWithin` (1, 5) -- 3 +/-2
            length b `shouldBeWithin` (5, 9) -- 7 +/-2

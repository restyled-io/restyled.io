module Restyled.TimeSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Time

spec :: Spec
spec = do
    describe "HasSeconds" $ do
        it "works for Days, Hours and Minutes" $ property $ \d ->
            let
                days = Days d
                hours = Hours $ 24 * d
                minutes = Minutes $ 24 * 60 * d
                seconds = Seconds $ 24 * 60 * 60 * d
            in
                toSeconds days
                == toSeconds hours
                && toSeconds hours
                == toSeconds minutes
                && toSeconds minutes
                == toSeconds seconds

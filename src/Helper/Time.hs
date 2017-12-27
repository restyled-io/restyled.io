module Helper.Time
    ( prettyDuration
    ) where

import Data.Time

-- | Show the duration between two times
--
-- >>> mkTime = parseTimeOrError False defaultTimeLocale "%F %T"
-- >>> mkDiff x y = prettyDuration (mkTime x) (mkTime y)
-- >>> mkDiff "2017-01-01 12:00:00" "2017-01-01 12:00:01"
-- "1s"
--
-- >>> mkDiff "2017-01-01 12:01:00" "2017-01-01 12:02:03"
-- "1m3s"
--
-- >>> mkDiff "2017-01-01 12:00:00" "2017-01-02 14:03:05"
-- "1d2h3m5s"
--
prettyDuration :: UTCTime -> UTCTime -> String
prettyDuration from to = showDuration $ round $ diffUTCTime to from
  where
    showDuration :: Integer -> String
    showDuration seconds
        | seconds >= 60*60*24 = helper seconds (60*60*24) "d"
        | seconds >= 60*60 = helper seconds (60*60) "h"
        | seconds >= 60 = helper seconds 60 "m"
        | otherwise = show seconds ++ "s"

    helper x y z =
        let (x', y') = x `divMod` y
        in show x' ++ z ++ showDuration y'

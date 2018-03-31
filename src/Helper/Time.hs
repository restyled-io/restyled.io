module Helper.Time
    ( prettyDuration
    ) where

import Prelude

import Control.Monad (guard)
import Data.Maybe (catMaybes)
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
    showDuration seconds = head $ catMaybes
        [ helper "d" seconds $ 60*60*24
        , helper "h" seconds $ 60*60
        , helper "m" seconds   60
        , Just $ show seconds ++ "s" -- head made safe by this
        ]

    helper label value threshold = do
        guard $ value >= threshold
        let (q, r) = value `divMod` threshold
        return $ show q ++ label ++ showDuration r

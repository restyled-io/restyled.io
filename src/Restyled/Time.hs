module Restyled.Time
    ( HasSeconds(..)
    , Days(Days)
    , Hours(Hours)
    , Minutes(Minutes)
    , Seconds(Seconds)
    , subtractTime
    )
where

import Restyled.Prelude

class HasSeconds t where
    toSeconds :: t -> Seconds

newtype Days = Days
    { unDays :: Int
    }

instance HasSeconds Days where
    toSeconds = toSeconds . Hours . (24 *) . unDays

newtype Hours = Hours
    { unHours :: Int
    }

instance HasSeconds Hours where
    toSeconds = toSeconds . Minutes . (60 *) . unHours

newtype Minutes = Minutes
    { unMinutes :: Int
    }

instance HasSeconds Minutes where
    toSeconds = toSeconds . Seconds . (60 *) . unMinutes

newtype Seconds = Seconds
    { unSeconds :: Int
    }
    deriving stock Eq

instance HasSeconds Seconds where
    toSeconds = id

subtractTime :: HasSeconds t => t -> UTCTime -> UTCTime
subtractTime = addUTCTime . negate . fromIntegral . unSeconds . toSeconds

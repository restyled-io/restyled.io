module GitHub.Data.Apps
    ( App
    , AppKey
    , unAppKey
    ) where

import Prelude

import Data.String

-- | Empty type, but allows for @'Id' 'App'@
data App

-- | RSA key content in PEM format, for authorizing via JWT
newtype AppKey = AppKey { unAppKey :: String }
    deriving newtype IsString

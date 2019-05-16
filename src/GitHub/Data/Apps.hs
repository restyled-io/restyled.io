module GitHub.Data.Apps
    ( App
    , Installation(..)
    )
where

import Prelude

import Data.Aeson
import GitHub.Data

data App

newtype Installation = Installation
    { installationId :: Id Installation
    }

instance FromJSON Installation where
    parseJSON = withObject "Installation" $ \o ->
        Installation <$> o .: "id"

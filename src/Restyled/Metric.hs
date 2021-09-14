module Restyled.Metric
    ( Metric(..)
    , Unit(..)
    , Dimension(..)
    ) where

import Restyled.Prelude

data Metric n = Metric
    { mName :: Text
    , mValue :: n
    , mUnit :: Unit
    , mDimensions :: [Dimension]
    }

instance ToJSON n => ToJSON (Metric n) where
    toJSON Metric {..} = object
        [ "MetricName" .= mName
        , "Value" .= mValue
        , "Unit" .= mUnit
        , "Dimensions" .= mDimensions
        ]

data Unit = Count | Percent

instance ToJSON Unit where
    toJSON = \case
        Count -> String "Count"
        Percent -> String "Percent"

data Dimension = Dimension
    { dName :: Text
    , dValue :: Text
    }

instance ToJSON Dimension where
    toJSON Dimension {..} = object ["Name" .= dName, "Value" .= dValue]

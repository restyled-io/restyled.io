module GitHub.Data.AccessTokens
    ( AccessToken(..)
    ) where

import Prelude

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)

data AccessToken = AccessToken
    { atToken :: Text
    , atExpiresAt :: UTCTime
    }

instance FromJSON AccessToken where
    parseJSON = withObject "GitHub.AccessToken"
        $ \o -> AccessToken <$> o .: "token" <*> o .: "expires_at"

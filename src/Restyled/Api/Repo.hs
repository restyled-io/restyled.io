module Restyled.Api.Repo
    ( ApiRepo(..)
    , apiRepo
    ) where

import Restyled.Prelude

import Restyled.Marketplace
import Restyled.Models.DB
import Restyled.RestylerImage
import Restyled.Settings (AppSettings(..))

data ApiRepo = ApiRepo
    { owner :: OwnerName
    , name :: RepoName
    , isPrivate :: Bool
    , isEnabled :: Bool
    , installationId :: InstallationId
    , marketplacePlanAllows :: Maybe MarketplacePlanAllows
    , restylerImage :: RestylerImage
    , restylerLogLevel :: ApiLogLevel
    }
    deriving stock (Show, Generic)
    deriving anyclass ToJSON

newtype ApiLogLevel = ApiLogLevel
    { unApiLogLevel :: LogLevel
    }
    deriving newtype Show

instance ToJSON ApiLogLevel where
    toJSON = toJSON . logLevelToText . unApiLogLevel
    toEncoding = toEncoding . logLevelToText . unApiLogLevel

apiRepo :: Entity Repo -> AppSettings -> Maybe MarketplacePlanAllows -> ApiRepo
apiRepo (Entity _ Repo {..}) AppSettings {..} mAllows = ApiRepo
    { owner = repoOwner
    , name = repoName
    , isPrivate = repoIsPrivate
    , isEnabled = repoEnabled
    , installationId = repoInstallationId
    , marketplacePlanAllows = mAllows
    , restylerImage = fromMaybe appRestylerImage repoRestylerImage
    , restylerLogLevel = ApiLogLevel
        $ if repoDebugEnabled then LevelDebug else appLogLevel
    }

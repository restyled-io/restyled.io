module Restyled.Api.Repo
    ( ApiRepo(..)
    , apiRepo
    ) where

import Restyled.Prelude

import Blammo.Logging.LogSettings (shouldLogLevel)
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
    , restylerLogFormat :: Text
    -- ^ TODO: 'LogFormat' needs 'ToJSON'
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
        $ if repoDebugEnabled then LevelDebug else guessLogLevel appLogSettings
    , restylerLogFormat = "json"
    }

logLevelToText :: LogLevel -> Text
logLevelToText = \case
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO"
    LevelWarn -> "WARN"
    LevelError -> "ERROR"
    LevelOther x -> x

guessLogLevel :: LogSettings -> LogLevel
guessLogLevel settings = fromMaybe (LevelOther "unknown")
    $ find shouldLog sortedLevels
  where
    shouldLog = shouldLogLevel settings ""
    sortedLevels =
        [LevelOther "trace", LevelDebug, LevelInfo, LevelWarn, LevelError]

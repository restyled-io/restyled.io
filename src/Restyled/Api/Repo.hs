module Restyled.Api.Repo
    ( ApiRepo(..)
    , apiRepo
    ) where

import Restyled.Prelude

import Blammo.Logging.LogSettings (getLogSettingsLevels)
import Blammo.Logging.LogSettings.LogLevels (newLogLevels, showLogLevels)
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
    , restylerEnv :: [Text]
    }
    deriving stock (Show, Generic)
    deriving anyclass ToJSON

apiRepo :: Entity Repo -> AppSettings -> Maybe MarketplacePlanAllows -> ApiRepo
apiRepo (Entity _ Repo {..}) AppSettings {..} mAllows = ApiRepo
    { owner = repoOwner
    , name = repoName
    , isPrivate = repoIsPrivate
    , isEnabled = repoEnabled
    , installationId = repoInstallationId
    , marketplacePlanAllows = mAllows -- Remove after agent is updated
    , restylerImage = fromMaybe appRestylerImage repoRestylerImage
    , restylerEnv =
        [ "REPO_DISABLED=" <> if repoEnabled then "" else "x"
        , "PLAN_RESTRICTION=" <> fromMaybe "" mPlanRestriction
        , "PLAN_UPGRADE_URL=https://github.com/marketplace/restyled-io"
        , "LOG_FORMAT=json"
        , "LOG_LEVEL=" <> pack (showLogLevels logLevels)
        ]
    }
  where
    mPlanRestriction =
        fmap (toPlanRestriction repoOwner) $ getLimitation =<< mAllows

    logLevels = if repoDebugEnabled
        then newLogLevels LevelDebug []
        else getLogSettingsLevels appLogSettings

toPlanRestriction :: OwnerName -> MarketplacePlanLimitation -> Text
toPlanRestriction ownerName = \case
    MarketplacePlanNotFound ->
        "No Marketplace plan for the owner of this repository ("
            <> toPathPart ownerName
            <> ")"
    MarketplacePlanPublicOnly ->
        "Marketplace plan for the owner of this repository ("
            <> toPathPart ownerName
            <> ") only allows public repositories"
    MarketplacePlanMaxRepos ->
        "You have reached the maximum number of private repositories for the Marketplace plan for the owner of this repository ("
            <> toPathPart ownerName
            <> ")"
    MarketplacePlanAccountExpired asOf ->
        "Marketplace plan for the owner of this repository ("
            <> toPathPart ownerName
            <> ") expired at "
            <> pack (show asOf)

getLimitation :: MarketplacePlanAllows -> Maybe MarketplacePlanLimitation
getLimitation = \case
    MarketplacePlanAllows -> Nothing
    MarketplacePlanForbids x -> Just x

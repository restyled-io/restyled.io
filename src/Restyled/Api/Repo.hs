module Restyled.Api.Repo
    ( ApiRepo(..)
    , apiRepo
    ) where

import Restyled.Prelude

import Blammo.Logging.LogSettings (getLogSettingsLevels)
import Blammo.Logging.LogSettings.LogLevels (newLogLevels, showLogLevels)
import Restyled.Bytes
import Restyled.CpuShares
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
    , restylerImage = fromMaybe appRestylerImage repoRestylerImage
    , restylerEnv = catMaybes
        [ Just $ "REPO_DISABLED=" <> if repoEnabled then "" else "x"
        , Just "LOG_FORMAT=json"
        , Just $ "LOG_LEVEL=" <> pack (showLogLevels logLevels)
        , Just "PLAN_UPGRADE_URL=https://github.com/marketplace/restyled-io"
        , ("PLAN_RESTRICTION=" <>)
        . toPlanRestriction repoOwner
        <$> (getLimitation =<< mAllows)
        , ("RESTYLER_CPU_SHARES=" <>)
        . cpuSharesToText
        <$> (marketplacePlanCpuShares =<< getPlan =<< mAllows)
        , ("RESTYLER_MEMORY=" <>)
        . bytesToText
        <$> (marketplacePlanMemory =<< getPlan =<< mAllows)
        ]
    }
  where
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

getPlan :: MarketplacePlanAllows -> Maybe MarketplacePlan
getPlan = \case
    MarketplacePlanAllows x -> x
    MarketplacePlanForbids{} -> Nothing

getLimitation :: MarketplacePlanAllows -> Maybe MarketplacePlanLimitation
getLimitation = \case
    MarketplacePlanAllows{} -> Nothing
    MarketplacePlanForbids x -> Just x

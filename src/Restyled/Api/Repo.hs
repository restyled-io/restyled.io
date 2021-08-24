module Restyled.Api.Repo
    ( ApiRepo(..)
    , apiRepo
    ) where

import Restyled.Prelude

import Restyled.Marketplace
import Restyled.Models.DB

data ApiRepo = ApiRepo
    { owner :: OwnerName
    , name :: RepoName
    , isPrivate :: Bool
    , installationId :: InstallationId
    , marketplacePlanAllows :: Maybe MarketplacePlanAllows
    -- ^ Only provided out by us, 'Nothing' when used as an input value
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

apiRepo :: Entity Repo -> Maybe MarketplacePlanAllows -> ApiRepo
apiRepo (Entity _ Repo {..}) mAllows = ApiRepo
    { owner = repoOwner
    , name = repoName
    , isPrivate = repoIsPrivate
    , installationId = repoInstallationId
    , marketplacePlanAllows = mAllows
    }

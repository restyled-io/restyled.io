module Restyled.Models.User
    (
    -- * Virtual properties
      userIsAdmin

    -- * Data access
    , fetchMarketplaceAccountForLoginT
    )
where

import Restyled.Prelude

import Restyled.Models.DB
import Restyled.Settings

userIsAdmin :: AppSettings -> User -> Bool
userIsAdmin AppSettings {..} = maybe False (`elem` appAdmins) . userEmail

fetchMarketplaceAccountForLoginT
    :: MonadIO m
    => GitHubUserName
    -> MaybeT (SqlPersistT m) (Entity MarketplaceAccount)
fetchMarketplaceAccountForLoginT = getByT . UniqueMarketplaceAccount

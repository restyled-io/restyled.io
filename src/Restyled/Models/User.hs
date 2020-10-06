module Restyled.Models.User
    (
    -- * Virtual properties
      userIsAdmin

    -- * Data access
    , fetchMarketplaceAccountForUserT
    , fetchMarketplaceAccountForLoginT
    )
where

import Restyled.Prelude

import Restyled.Models.DB
import Restyled.Settings

userIsAdmin :: AppSettings -> User -> Bool
userIsAdmin AppSettings {..} = maybe False (`elem` appAdmins) . userEmail

fetchMarketplaceAccountForUserT
    :: MonadIO m => User -> MaybeT (SqlPersistT m) (Entity MarketplaceAccount)
fetchMarketplaceAccountForUserT User {..} = do
    githubLogin <- hoistMaybe userGithubUsername
    fetchMarketplaceAccountForLoginT githubLogin

fetchMarketplaceAccountForLoginT
    :: MonadIO m
    => GitHubUserName
    -> MaybeT (SqlPersistT m) (Entity MarketplaceAccount)
fetchMarketplaceAccountForLoginT username =
    selectFirstT [MarketplaceAccountGithubLogin ==. username] []

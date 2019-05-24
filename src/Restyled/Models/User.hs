module Restyled.Models.User
    (
    -- * Virtual properties
      userIsAdmin

    -- * Data access
    , fetchMarketplacePlanForUser
    , fetchMarketplacePlanByLogin
    )
where

import Restyled.Prelude

import Restyled.Models.DB
import Restyled.Settings

userIsAdmin :: AppSettings -> User -> Bool
userIsAdmin AppSettings {..} = maybe False (`elem` appAdmins) . userEmail

fetchMarketplacePlanForUser
    :: MonadIO m => User -> SqlPersistT m (Maybe MarketplacePlan)
fetchMarketplacePlanForUser User {..} = runMaybeT $ do
    githubId <- hoistMaybe userGithubUserId
    githubLogin <- hoistMaybe userGithubUsername
    account <- MaybeT $ getBy $ UniqueMarketplaceAccount githubId githubLogin
    MaybeT $ get $ marketplaceAccountMarketplacePlan $ entityVal account

fetchMarketplacePlanByLogin
    :: MonadIO m => GitHubUserName -> SqlPersistT m (Maybe MarketplacePlan)
fetchMarketplacePlanByLogin username = runMaybeT $ do
    account <- MaybeT
        $ selectFirst [MarketplaceAccountGithubLogin ==. username] []
    MaybeT $ get $ marketplaceAccountMarketplacePlan $ entityVal account

module Model.User
    (
    -- * Virtual properties
      userIsAdmin

    -- * Data access
    , fetchMarketplacePlanForUser
    , fetchMarketplacePlanByLogin
    )
where

import ClassyPrelude

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model
import Settings

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

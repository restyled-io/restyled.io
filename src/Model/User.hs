module Model.User
    ( fetchMarketplacePlan
    )
where

import ClassyPrelude

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model

fetchMarketplacePlan
    :: MonadIO m => Entity User -> SqlPersistT m (Maybe MarketplacePlan)
fetchMarketplacePlan user = runMaybeT $ do
    planId <- hoistMaybe $ userMarketplacePlan $ entityVal user
    MaybeT $ get planId

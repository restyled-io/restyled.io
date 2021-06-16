module Restyled.Handlers.Admin.Marketplace.Plans
    ( getAdminMarketplacePlansR
    ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.Yesod

newtype MarketplacePlansQuery = MarketplacePlansQuery
    { mpqGitHubId :: Int
    }

queryForm :: FormInput Handler MarketplacePlansQuery
queryForm = MarketplacePlansQuery <$> ireq intField "githubId"

queryFilters :: MarketplacePlansQuery -> [Filter MarketplacePlan]
queryFilters MarketplacePlansQuery {..} =
    [MarketplacePlanGithubId ==. githubId]
  where
    -- Pass githubId=-1 to query for non-GH Plans
    githubId = case mpqGitHubId of
        -1 -> Nothing
        x -> Just x

querySelectOpts :: MarketplacePlansQuery -> [SelectOpt MarketplacePlan]
querySelectOpts _ = [Asc MarketplacePlanName]

getAdminMarketplacePlansR :: Handler Value
getAdminMarketplacePlansR = do
    query <- runInputGet queryForm
    runDB $ toJSON <$> selectList (queryFilters query) (querySelectOpts query)

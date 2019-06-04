module GitHub.Endpoints.MarketplaceListing.Plans.Accounts
    ( MarketplaceAccount(..)
    , marketplaceListingPlanAccounts
    , marketplaceListingPlanAccountsR
    )
where

import Prelude

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Vector (Vector)
import GitHub.Auth
import GitHub.Data
import GitHub.Endpoints.MarketplaceListing.Plans
import GitHub.Request

data MarketplaceAccount = MarketplaceAccount
    { marketplaceAccountId :: Id User
    , marketplaceAccountLogin :: Name User
    }

instance FromJSON MarketplaceAccount where
    parseJSON = withObject "Account" $ \o -> MarketplaceAccount
        <$> o .: "id"
        <*> o .: "login"

marketplaceListingPlanAccounts
    :: AuthMethod am
    => am
    -> Bool -- ^ Use @stubbed@ API?
    -> Id MarketplacePlan
    -> IO (Either Error (Vector MarketplaceAccount))
marketplaceListingPlanAccounts auth useStubbed planId =
    executeRequest auth
        $ marketplaceListingPlanAccountsR useStubbed planId FetchAll

marketplaceListingPlanAccountsR
    :: Bool
    -> Id MarketplacePlan
    -> FetchCount
    -> Request k (Vector MarketplaceAccount)
marketplaceListingPlanAccountsR useStubbed planId = pagedQuery path []
  where
    path = concat
        [ ["marketplace_listing"]
        , [ "stubbed" | useStubbed ]
        , ["plans", toPathPart planId, "accounts"]
        ]

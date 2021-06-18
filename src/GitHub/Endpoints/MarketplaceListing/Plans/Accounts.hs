module GitHub.Endpoints.MarketplaceListing.Plans.Accounts
    ( MarketplaceAccount(..)
    , MarketplacePurchase(..)
    , marketplaceListingPlanAccounts
    , marketplaceListingPlanAccountsR
    ) where

import Prelude

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GitHub.Auth
import GitHub.Data
import GitHub.Endpoints.MarketplaceListing.Plans
import GitHub.Request

data MarketplaceAccount = MarketplaceAccount
    { marketplaceAccountId :: Id User
    , marketplaceAccountLogin :: Name User
    , marketplaceAccountEmail :: Maybe Text
    , marketplaceAccountOrganizationBillingEmail :: Maybe Text
    , marketplaceAccountType :: Text
    , marketplaceAccountMarketplacePurchase :: MarketplacePurchase
    }

-- brittany-disable-next-binding

instance FromJSON MarketplaceAccount where
    parseJSON = withObject "Account" $ \o -> MarketplaceAccount
        <$> o .: "id"
        <*> o .: "login"
        <*> o .:? "email"
        <*> o .:? "organization_billing_email"
        <*> o .: "type"
        <*> o .: "marketplace_purchase"

data MarketplacePurchase = MarketplacePurchase
    { marketplacePurchaseBillingCycle :: Text
    , marketplacePurchaseNextBillingDate :: Maybe UTCTime
    , marketplacePurchaseOnFreeTrial :: Bool
    , marketplacePurchaseFreeTrialEndsOn :: Maybe UTCTime
    }

-- brittany-disable-next-binding
--
instance FromJSON MarketplacePurchase where
    parseJSON = withObject "Purchase" $ \o -> MarketplacePurchase
        <$> o .: "billing_cycle"
        <*> o .:? "next_billing_date"
        <*> o .: "on_free_trial"
        <*> o .:? "free_trial_ends_on"

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

module GitHub.Endpoints.MarketplaceListing.Plans
    ( MarketplacePlan(..)
    , marketplaceListingPlans
    , marketplaceListingPlansR
    )
where

import Prelude

import Data.Aeson (FromJSON(..), withObject, (.:))
import Data.Text (Text)
import Data.Vector (Vector)
import GitHub.Auth
import GitHub.Data
import GitHub.Request
import Numeric.Natural

data MarketplacePlan = MarketplacePlan
    { marketplacePlanId :: Id MarketplacePlan
    , marketplacePlanName :: Text
    , marketplacePlanDescription :: Text
    , marketplacePlanState :: Text
    , marketplacePlanMonthlyPriceInCents :: Natural
    }

instance FromJSON MarketplacePlan where
    parseJSON = withObject "Plan" $ \o ->
        MarketplacePlan
            <$> o
            .: "id"
            <*> o
            .: "name"
            <*> o
            .: "description"
            <*> o
            .: "state"
            <*> o
            .: "monthly_price_in_cents"

marketplaceListingPlans
    :: AuthMethod am
    => am
    -> Bool -- ^ Use @stubbed@ API?
    -> IO (Either Error (Vector MarketplacePlan))
marketplaceListingPlans auth useStubbed =
    executeRequest auth $ marketplaceListingPlansR useStubbed FetchAll

marketplaceListingPlansR
    :: Bool -> FetchCount -> Request k (Vector MarketplacePlan)
marketplaceListingPlansR useStubbed = pagedQuery path []
  where
    path = "marketplace_listing" : [ "stubbed" | useStubbed ] <> ["plans"]

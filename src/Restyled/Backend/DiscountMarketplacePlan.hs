module Restyled.Backend.DiscountMarketplacePlan
    ( fetchDiscountMarketplacePlan
    , fetchUserHasDiscountMarketplacePlan
    , giftDiscountMarketplacePlan
    , ungiftDiscountMarketplacePlan
    )
where

import Restyled.Prelude

import Restyled.Models
import Restyled.PrivateRepoAllowance

fetchDiscountMarketplacePlan
    :: MonadIO m => SqlPersistT m (Entity MarketplacePlan)
fetchDiscountMarketplacePlan = upsert
    plan
    [ MarketplacePlanName =. marketplacePlanName
    , MarketplacePlanDescription =. marketplacePlanDescription
    ]
  where
    plan@MarketplacePlan {..} = MarketplacePlan
        { marketplacePlanGithubId = Nothing
        , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceUnlimited
        , marketplacePlanName = "Friends & Family"
        , marketplacePlanDescription = "Manually managed discount plan"
        }

fetchUserHasDiscountMarketplacePlan :: MonadIO m => User -> SqlPersistT m Bool
fetchUserHasDiscountMarketplacePlan user =
    fmap (fromMaybe False) $ runMaybeT $ do
        planId <- lift $ entityKey <$> fetchDiscountMarketplacePlan
        Entity _ MarketplaceAccount {..} <- fetchMarketplaceAccountForUserT user
        pure $ marketplaceAccountMarketplacePlan == planId

giftDiscountMarketplacePlan
    :: MonadIO m => GitHubUserId -> GitHubUserName -> SqlPersistT m ()
giftDiscountMarketplacePlan githubId githubLogin = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan

    void $ upsert
        MarketplaceAccount
            { marketplaceAccountGithubId = githubId
            , marketplaceAccountGithubLogin = githubLogin
            , marketplaceAccountMarketplacePlan = planId
            , marketplaceAccountGithubType = "User"
            , marketplaceAccountEmail = Nothing
            , marketplaceAccountBillingEmail = Nothing
            }
        [MarketplaceAccountMarketplacePlan =. planId]

ungiftDiscountMarketplacePlan
    :: MonadIO m => GitHubUserId -> GitHubUserName -> SqlPersistT m ()
ungiftDiscountMarketplacePlan githubId githubLogin = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan

    deleteWhere
        [ MarketplaceAccountGithubId ==. githubId
        , MarketplaceAccountGithubLogin ==. githubLogin
        , MarketplaceAccountMarketplacePlan ==. planId
        ]

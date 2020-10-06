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
fetchDiscountMarketplacePlan = do
    -- Unsafe UPSERT required because github_id is a nullable index. Should be
    -- fine since we always expect this to exist.
    mPlan <- selectFirst
        [ MarketplacePlanGithubId ==. marketplacePlanGithubId
        , MarketplacePlanName ==. marketplacePlanName
        , MarketplacePlanDescription ==. marketplacePlanDescription
        ]
        []

    maybe (insertEntity plan) pure mPlan
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
            { marketplaceAccountGithubId = Just githubId
            , marketplaceAccountGithubLogin = githubLogin
            , marketplaceAccountMarketplacePlan = planId
            , marketplaceAccountGithubType = "User"
            , marketplaceAccountEmail = Nothing
            , marketplaceAccountBillingEmail = Nothing
            }
        [MarketplaceAccountMarketplacePlan =. planId]

ungiftDiscountMarketplacePlan :: MonadIO m => GitHubUserName -> SqlPersistT m ()
ungiftDiscountMarketplacePlan githubLogin = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan

    deleteWhere
        [ MarketplaceAccountGithubLogin ==. githubLogin
        , MarketplaceAccountMarketplacePlan ==. planId
        ]

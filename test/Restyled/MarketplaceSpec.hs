module Restyled.MarketplaceSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Marketplace
import Restyled.PrivateRepoAllowance
import Restyled.Time

spec :: Spec
spec = withApp $ do
    describe "marketplacePlanAllows" $ do
        it "always allows public repos" $ runDB $ do
            repo <- insertRepo "restyled-io" "restyled.io" False
            shouldAllow repo

        it "allows private repos on the unlimited plan" $ runDB $ do
            insertAccountOnPlan
                "restyled-io"
                (Just 2553)
                Nothing
                PrivateRepoAllowanceUnlimited
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldAllow repo

        it "disallows private repos on expired plans" $ runDB $ do
            expiredAt <- subtractTime (Days 1) <$> getCurrentTime
            insertAccountOnPlan
                "restyled-io"
                (Just 2553)
                (Just expiredAt)
                PrivateRepoAllowanceUnlimited
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldForbidExpired repo expiredAt

        it "disallows private repos without a plan" $ runDB $ do
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldForbid repo MarketplacePlanNotFound

        it "disallows private repos on a public plan" $ runDB $ do
            insertAccountOnPlan
                "restyled-io"
                Nothing
                Nothing
                PrivateRepoAllowanceNone
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldForbid repo MarketplacePlanPublicOnly

        it "limits repos on a limited plan" $ runDB $ do
            insertAccountOnPlan "restyled-io" (Just 2695) Nothing
                $ PrivateRepoAllowanceLimited 1
            privateRepo1 <- insertRepo "restyled-io" "restyled.io" True
            privateRepo2 <- insertRepo "restyled-io" "restyler" True
            ossRepo <- insertRepo "restyled-io" "demo" False

            shouldAllow privateRepo1
            shouldAllow ossRepo
            shouldForbid privateRepo2 MarketplacePlanMaxRepos
            shouldAllow privateRepo1
            shouldForbid privateRepo2 MarketplacePlanMaxRepos

shouldAllow :: (HasCallStack, MonadIO m) => Entity Repo -> SqlPersistT m ()
shouldAllow repo = do
    allows <- marketplacePlanAllows repo
    allows `shouldBe` MarketplacePlanAllows

shouldForbid
    :: (HasCallStack, MonadIO m)
    => Entity Repo
    -> MarketplacePlanLimitation
    -> SqlPersistT m ()
shouldForbid repo with = do
    allows <- marketplacePlanAllows repo
    allows `shouldBe` MarketplacePlanForbids with

shouldForbidExpired
    :: (HasCallStack, MonadIO m) => Entity Repo -> UTCTime -> SqlPersistT m ()
shouldForbidExpired repo expiredAt = do
    allows <- marketplacePlanAllows repo

    -- We want to check for the Expired case without actually comparing the
    -- expiredAt value, since we might fail on ms differences. In the unexpected
    -- case, we only use shouldBe with that value to provide an informative
    -- message.
    case allows of
        MarketplacePlanForbids (MarketplacePlanAccountExpired _) -> pure ()
        x -> x `shouldBe` expected
  where
    expected = MarketplacePlanForbids $ MarketplacePlanAccountExpired expiredAt

insertRepo
    :: MonadIO m
    => OwnerName
    -> RepoName
    -> Bool -- ^ Is private?
    -> SqlPersistT m (Entity Repo)
insertRepo owner name isPrivate = insertEntity Repo
    { repoSvcs = GitHubSVCS
    , repoOwner = owner
    , repoName = name
    , repoInstallationId = 123
    , repoIsPrivate = isPrivate
    , repoDebugEnabled = False
    , repoEnabled = True
    , repoRestylerImage = Nothing
    }

insertAccountOnPlan
    :: MonadIO m
    => GitHubUserName
    -> Maybe Int
    -> Maybe UTCTime
    -> PrivateRepoAllowance
    -> SqlPersistT m ()
insertAccountOnPlan username mPlanGitHubId mExpiresAt planAllowance = do
    planId <- insert MarketplacePlan
        { marketplacePlanGithubId = mPlanGitHubId
        , marketplacePlanPrivateRepoAllowance = planAllowance
        , marketplacePlanName = ""
        , marketplacePlanDescription = ""
        , marketplacePlanMonthlyRevenue = fromCents 0
        , marketplacePlanRetired = False
        }
    insert_ MarketplaceAccount
        { marketplaceAccountGithubId = Nothing
        , marketplaceAccountGithubLogin = username
        , marketplaceAccountMarketplacePlan = planId
        , marketplaceAccountGithubType = "User"
        , marketplaceAccountEmail = Nothing
        , marketplaceAccountBillingEmail = Nothing
        , marketplaceAccountExpiresAt = mExpiresAt
        }

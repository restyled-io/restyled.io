module Restyled.Backend.MarketplaceSpec
    ( spec
    )
where

import Restyled.Test

import Restyled.Backend.Marketplace

spec :: Spec
spec = withApp $ do
    describe "marketplacePlanAllows" $ do
        let shouldAllow repo = do
                allows <- marketplacePlanAllows repo
                allows `shouldBe` MarketplacePlanAllows
            shouldForbid repo with = do
                allows <- marketplacePlanAllows repo
                allows `shouldBe` MarketplacePlanForbids with

        it "always allows public repos" $ runDB $ do
            repo <- insertRepo "restyled-io" "restyled.io" False
            shouldAllow repo

        it "allows private repos on the unlimited plan" $ runDB $ do
            insertAccountOnPlan "restyled-io" 2553
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldAllow repo

        it "disallows private repos without a plan" $ runDB $ do
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldForbid repo MarketplacePlanNotFound

        it "disallows private repos on other (assumed public) plan" $ runDB $ do
            insertAccountOnPlan "restyled-io" 1
            repo <- insertRepo "restyled-io" "restyled.io" True
            shouldForbid repo MarketplacePlanPublicOnly

        it "limits repos on a limited plan" $ runDB $ do
            insertAccountOnPlan "restyled-io" 2695
            privateRepo1 <- insertRepo "restyled-io" "restyled.io" True
            privateRepo2 <- insertRepo "restyled-io" "restyler" True
            ossRepo <- insertRepo "restyled-io" "demo" False

            shouldAllow privateRepo1
            shouldAllow ossRepo
            shouldForbid privateRepo2 MarketplacePlanMaxRepos
            shouldAllow privateRepo1
            shouldForbid privateRepo2 MarketplacePlanMaxRepos

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

insertAccountOnPlan :: MonadIO m => GitHubUserName -> Int -> SqlPersistT m ()
insertAccountOnPlan username planGitHubId = do
    planId <- insert MarketplacePlan
        { marketplacePlanGithubId = planGitHubId
        , marketplacePlanName = ""
        , marketplacePlanDescription = ""
        }
    insert_ MarketplaceAccount
        { marketplaceAccountGithubId = 123
        , marketplaceAccountGithubLogin = username
        , marketplaceAccountMarketplacePlan = planId
        , marketplaceAccountGithubType = "User"
        , marketplaceAccountEmail = Nothing
        , marketplaceAccountBillingEmail = Nothing
        }

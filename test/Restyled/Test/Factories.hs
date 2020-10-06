module Restyled.Test.Factories
    ( buildRepo
    , buildPrivateRepo
    , buildPrivateMarketplacePlan
    , buildMarketplaceAccount
    )
where

import Restyled.Prelude

import Restyled.Models
import Restyled.PrivateRepoAllowance

buildRepo :: OwnerName -> RepoName -> Repo
buildRepo owner name = Repo
    { repoSvcs = GitHubSVCS
    , repoOwner = owner
    , repoName = name
    , repoInstallationId = 123
    , repoIsPrivate = False
    , repoDebugEnabled = False
    , repoEnabled = True
    , repoRestylerImage = Nothing
    }

buildPrivateRepo :: OwnerName -> RepoName -> Repo
buildPrivateRepo owner name = Repo
    { repoSvcs = GitHubSVCS
    , repoOwner = owner
    , repoName = name
    , repoInstallationId = 123
    , repoIsPrivate = True
    , repoDebugEnabled = False
    , repoEnabled = True
    , repoRestylerImage = Nothing
    }

-- | TODO: this should be Fixture, not Factory because it's a singleton
buildPrivateMarketplacePlan :: MarketplacePlan
buildPrivateMarketplacePlan = MarketplacePlan
    { marketplacePlanGithubId = Just 2695
    , marketplacePlanPrivateRepoAllowance = PrivateRepoAllowanceLimited 1
    , marketplacePlanName = "Solo"
    , marketplacePlanDescription = ""
    }

buildMarketplaceAccount
    :: Maybe GitHubUserId -> GitHubUserName -> MarketplacePlanId -> MarketplaceAccount
buildMarketplaceAccount mUserId userName planId = MarketplaceAccount
    { marketplaceAccountGithubId = mUserId
    , marketplaceAccountGithubLogin = userName
    , marketplaceAccountMarketplacePlan = planId
    , marketplaceAccountGithubType = "User"
    , marketplaceAccountEmail = Nothing
    , marketplaceAccountBillingEmail = Nothing
    }

module Restyled.Api.CreateRepoSpec
    ( spec
    ) where

import Restyled.Test

import Restyled.Api.CreateRepo
import Restyled.Api.Repo (ApiRepo(ApiRepo))
import qualified Restyled.Api.Repo as ApiRepo
import Restyled.Marketplace

spec :: Spec
spec = withApp $ do
    describe "findOrCreateRepo" $ do
        let
            body = ApiRepo
                { ApiRepo.owner = "foo"
                , ApiRepo.name = "bar"
                , ApiRepo.isPrivate = False
                , ApiRepo.installationId = 1
                , ApiRepo.marketplacePlanAllows = Nothing
                }

        it "has upsert semantics" $ runDB $ do
            assertValidateT $ do
                void $ findOrCreateRepo body
                void $ findOrCreateRepo body
                void $ findOrCreateRepo body

            repos <- selectList
                [ RepoOwner ==. ApiRepo.owner body
                , RepoName ==. ApiRepo.name body
                ]
                []
            repos `shouldSatisfy` (== 1) . length

        it "updates installationId and isPrivate" $ runDB $ do
            insert_ Repo
                { repoSvcs = GitHubSVCS
                , repoOwner = ApiRepo.owner body
                , repoName = ApiRepo.name body
                , repoIsPrivate = True
                , repoInstallationId = 2
                , repoEnabled = True
                , repoDebugEnabled = False
                , repoRestylerImage = Nothing
                }

            void $ assertValidateT $ findOrCreateRepo body

            Just (Entity _ repo) <- getBy $ UniqueRepo
                GitHubSVCS
                (ApiRepo.owner body)
                (ApiRepo.name body)
            repoIsPrivate repo `shouldBe` False
            repoInstallationId repo `shouldBe` 1

        context "marketplacePlanAllows" $ do
            it "allows public repos" $ runDB $ do
                Right repo <- runValidateT
                    $ findOrCreateRepo body { ApiRepo.isPrivate = False }

                ApiRepo.marketplacePlanAllows repo
                    `shouldBe` Just MarketplacePlanAllows

            it "handles private plans" $ runDB $ do
                planId <- insert buildPrivateMarketplacePlan -- allowance=1
                insert_ $ buildMarketplaceAccount
                    Nothing
                    (nameToName $ ApiRepo.owner body)
                    planId

                Right (repo1, repo2) <-
                    runValidateT
                    $ (,)
                    <$> findOrCreateRepo body
                            { ApiRepo.name = "one"
                            , ApiRepo.isPrivate = True
                            }
                    <*> findOrCreateRepo body
                            { ApiRepo.name = "two"
                            , ApiRepo.isPrivate = True
                            }

                ApiRepo.marketplacePlanAllows repo1
                    `shouldBe` Just MarketplacePlanAllows
                ApiRepo.marketplacePlanAllows repo2 `shouldBe` Just
                    (MarketplacePlanForbids MarketplacePlanMaxRepos)

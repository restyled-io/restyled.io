module Restyled.Api.UpsertRepoSpec
    ( spec
    ) where

import Restyled.Test hiding (upsertRepo)

import qualified Data.Text as T
import Restyled.Api.Repo (ApiRepo)
import qualified Restyled.Api.Repo as ApiRepo
import Restyled.Api.UpsertRepo (ApiUpsertRepo(ApiUpsertRepo), upsertRepo)
import qualified Restyled.Api.UpsertRepo as ApiUpsertRepo
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
    describe "upsertRepo" $ do
        let
            body = ApiUpsertRepo
                { ApiUpsertRepo.owner = "foo"
                , ApiUpsertRepo.name = "bar"
                , ApiUpsertRepo.isPrivate = False
                , ApiUpsertRepo.installationId = 1
                }

        it "has upsert semantics" $ runDB $ do
            assertValidateT $ do
                void $ upsertRepo body
                void $ upsertRepo body
                void $ upsertRepo body

            repos <- selectList
                [ RepoOwner ==. ApiUpsertRepo.owner body
                , RepoName ==. ApiUpsertRepo.name body
                ]
                []
            repos `shouldSatisfy` (== 1) . length

        it "updates installationId and isPrivate" $ graph $ do
            void
                $ node @Repo ()
                $ edit
                $ (fieldLens RepoOwner .~ ApiUpsertRepo.owner body)
                . (fieldLens RepoName .~ ApiUpsertRepo.name body)
                . (fieldLens RepoIsPrivate .~ True)
                . (fieldLens RepoInstallationId .~ 2)

            lift $ runDB $ do
                void $ assertValidateT $ upsertRepo body

                Just (Entity _ repo) <- getBy $ UniqueRepo
                    GitHubSVCS
                    (ApiUpsertRepo.owner body)
                    (ApiUpsertRepo.name body)
                repoIsPrivate repo `shouldBe` False
                repoInstallationId repo `shouldBe` 1

        context "restylerEnv" $ do
            it "allows public repos" $ runDB $ do
                Right repo <- runValidateT
                    $ upsertRepo body { ApiUpsertRepo.isPrivate = False }

                lookupRestylerEnv "PLAN_RESTRICTION" repo `shouldBe` Nothing

            it "handles private plans" $ graph $ do
                plan <- node @MarketplacePlan () $ edit $ setPlanLimited 1
                void
                    $ node @MarketplaceAccount
                          ( nameToName $ ApiUpsertRepo.owner body
                          , entityKey plan
                          )
                    $ edit setAccountUnexpired

                Right (repo1, repo2) <-
                    lift
                    $ runDB
                    $ runValidateT
                    $ (,)
                    <$> upsertRepo body
                            { ApiUpsertRepo.name = "one"
                            , ApiUpsertRepo.isPrivate = True
                            }
                    <*> upsertRepo body
                            { ApiUpsertRepo.name = "two"
                            , ApiUpsertRepo.isPrivate = True
                            }

                lookupRestylerEnv "PLAN_RESTRICTION" repo1 `shouldBe` Nothing
                lookupRestylerEnv "PLAN_RESTRICTION" repo2
                    `shouldBe` Just
                                   "You have reached the maximum number of private repositories for the Marketplace plan for the owner of this repository (foo)"

lookupRestylerEnv :: Text -> ApiRepo -> Maybe Text
lookupRestylerEnv k =
    fmap (T.drop (T.length k + 1))
        . find ((k <> "=") `T.isPrefixOf`)
        . ApiRepo.restylerEnv

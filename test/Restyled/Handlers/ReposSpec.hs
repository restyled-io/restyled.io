{-# LANGUAGE QuasiQuotes #-}

module Restyled.Handlers.ReposSpec
    ( spec
    ) where

import Restyled.Test

import Control.Lens ((^?!))
import Data.Aeson.Lens
import Data.Aeson.QQ
import Restyled.Api.Repo (ApiRepo(ApiRepo))
import qualified Restyled.Api.Repo as ApiRepo

spec :: Spec
spec = withApp $ do
    describe "GET gh/:owner/repos/:repo" $ do
        itRequiresRepositoryAccess RepoR

    describe "GET gh/:owner/repos/:repo/jobs" $ do
        itRequiresRepositoryAccess jobsR

    describe "GET gh/:owner/repos/:repo/pulls/:number" $ do
        itRequiresRepositoryAccess $ pullR 1

    describe "GET gh/:owner/repos/:repo/pulls/:number/jobs" $ do
        itRequiresRepositoryAccess $ pullJobsR 1

    describe "PUT gh/:owner/repos/:repo" $ do
        it "validates owner matches" $ do
            void authenticateAsAdmin

            putJSON (repoP "baz" "bar" RepoR) $ ApiRepo
                { ApiRepo.owner = "foo"
                , ApiRepo.name = "bar"
                , ApiRepo.isPrivate = False
                , ApiRepo.installationId = 1
                , ApiRepo.marketplacePlanAllows = Nothing
                }

            statusIs 400
            resp <- getJsonBody
            resp `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "UnexpectedOwnerName"
                      , "contents": {"expected": "baz"}
                      }
                    ]
                }
            |]

        it "validates name matches" $ do
            void authenticateAsAdmin

            putJSON (repoP "foo" "quiix" RepoR) $ ApiRepo
                { ApiRepo.owner = "foo"
                , ApiRepo.name = "bar"
                , ApiRepo.isPrivate = False
                , ApiRepo.installationId = 1
                , ApiRepo.marketplacePlanAllows = Nothing
                }

            statusIs 400
            resp <- getJsonBody
            resp `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "UnexpectedRepoName"
                      , "contents": {"expected": "quiix"}
                      }
                    ]
                }
            |]

        it "accumulates validations" $ do
            void authenticateAsAdmin

            putJSON (repoP "bat" "quiix" RepoR) $ ApiRepo
                { ApiRepo.owner = "foo"
                , ApiRepo.name = "bar"
                , ApiRepo.isPrivate = False
                , ApiRepo.installationId = 1
                , ApiRepo.marketplacePlanAllows = Nothing
                }

            statusIs 400
            resp <- getJsonBody
            resp `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "UnexpectedOwnerName"
                      , "contents": {"expected": "bat"}
                      }
                    , { "tag": "UnexpectedRepoName"
                      , "contents": {"expected": "quiix"}
                      }
                    ]
                }
            |]

        -- N.B. This is just a smoke-test, more tests against findOrCreateRepo
        it "inserts a new repository and parrots back" $ do
            let owner :: OwnerName
                owner = "foo"
                name :: RepoName
                name = "bar"
                body = ApiRepo
                    { ApiRepo.owner = owner
                    , ApiRepo.name = name
                    , ApiRepo.isPrivate = False
                    , ApiRepo.installationId = 1
                    , ApiRepo.marketplacePlanAllows = Nothing
                    }

            void authenticateAsAdmin
            putJSON (repoP owner name RepoR) body

            statusIs 200
            resp <- getJsonBody
            resp ^?! key "owner" . _JSON `shouldBe` ApiRepo.owner body
            resp ^?! key "name" . _JSON `shouldBe` ApiRepo.name body
            resp ^?! key "isPrivate" . _JSON `shouldBe` ApiRepo.isPrivate body
            resp
                ^?! key "installationId"
                . _JSON
                `shouldBe` ApiRepo.installationId body
            resp
                ^? key "marketplacePlanAllows"
                . key "tag"
                . _String
                `shouldBe` Just "MarketplacePlanAllows"
            Just (Entity _ Repo {..}) <- runDB $ getBy $ UniqueRepo
                GitHubSVCS
                owner
                name
            repoOwner `shouldBe` ApiRepo.owner body
            repoName `shouldBe` ApiRepo.name body
            repoIsPrivate `shouldBe` ApiRepo.isPrivate body
            repoInstallationId `shouldBe` ApiRepo.installationId body

itRequiresRepositoryAccess :: RepoP -> YesodSpec App ()
itRequiresRepositoryAccess path = do
    it "404s for unknown repos" $ do
        get $ repoP owner name path

        statusIs 404

    it "200s for public repos" $ do
        runDB $ insert_ $ publicRepo owner name

        get $ repoP owner name path

        statusIs 200

    it "404s for inaccessible repos, so as not to leak existence" $ do
        runDB $ insert_ $ makeInaccessible $ publicRepo owner name

        get $ repoP owner name path

        statusIs 404
  where
    owner :: OwnerName
    owner = "foo"

    name :: RepoName
    name = "bar"

publicRepo :: OwnerName -> RepoName -> Repo
publicRepo owner name = Repo
    { repoSvcs = GitHubSVCS
    , repoOwner = owner
    , repoName = name
    , repoInstallationId = 1
    , repoIsPrivate = False
    , repoDebugEnabled = False
    , repoEnabled = True
    , repoRestylerImage = Nothing
    }

makeInaccessible :: Repo -> Repo
makeInaccessible repo = repo { repoIsPrivate = True }

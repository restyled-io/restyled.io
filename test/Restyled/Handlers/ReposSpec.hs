{-# LANGUAGE QuasiQuotes #-}

module Restyled.Handlers.ReposSpec
  ( spec
  ) where

import Restyled.Test

import Data.Aeson.Lens
import Data.Aeson.QQ
import Lens.Micro ((^?!))
import Restyled.Test.Graphula

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
    it "validates owner matches" $ graph $ do
      admin <- genAdmin
      lift $ do
        authenticateAs admin

        putJSON
          (repoP "baz" "bar" RepoR)
          [aesonQQ|
                        { "owner": "foo"
                        , "name": "bar"
                        , "isPrivate": false
                        , "installationId": 1
                        }
                    |]

        statusIs 400
        resp <- getJsonBody
        resp
          `shouldMatchJson` [aesonQQ|
                    { "errors":
                        [ { "tag": "UnexpectedOwnerName"
                          , "contents": {"expected": "baz"}
                          }
                        ]
                    }
                |]

    it "validates name matches" $ graph $ do
      admin <- genAdmin
      lift $ do
        authenticateAs admin

        putJSON
          (repoP "foo" "quiix" RepoR)
          [aesonQQ|
                        { "owner": "foo"
                        , "name": "bar"
                        , "isPrivate": false
                        , "installationId": 1
                        }
                    |]

        statusIs 400
        resp <- getJsonBody
        resp
          `shouldMatchJson` [aesonQQ|
                    { "errors":
                        [ { "tag": "UnexpectedRepoName"
                          , "contents": {"expected": "quiix"}
                          }
                        ]
                    }
                |]

    it "accumulates validations" $ graph $ do
      admin <- genAdmin

      lift $ do
        authenticateAs admin

        putJSON
          (repoP "bat" "quiix" RepoR)
          [aesonQQ|
                        { "owner": "foo"
                        , "name": "bar"
                        , "isPrivate": false
                        , "installationId": 1
                        }
                    |]

        statusIs 400
        resp <- getJsonBody
        resp
          `shouldMatchJson` [aesonQQ|
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
    it "creates a new repository and parrots back" $ graph $ do
      admin <- genAdmin

      lift $ do
        authenticateAs admin

        putJSON
          (repoP "foo" "bar" RepoR)
          [aesonQQ|
                        { "owner": "foo"
                        , "name": "bar"
                        , "isPrivate": false
                        , "installationId": 1
                        }
                    |]

        statusIs 200
        resp <- getJsonBody
        resp ^?! key "owner" . _JSON @_ @OwnerName `shouldBe` "foo"
        resp ^?! key "name" . _JSON @_ @RepoName `shouldBe` "bar"
        resp ^?! key "isPrivate" . _Bool `shouldBe` False
        resp
          ^?! key "installationId"
            . _JSON @_ @InstallationId
            `shouldBe` 1
        Just (Entity _ Repo {..}) <-
          runDB
            $ getBy
            $ UniqueRepo
              GitHubSVCS
              "foo"
              "bar"
        repoOwner `shouldBe` "foo"
        repoName `shouldBe` "bar"
        repoIsPrivate `shouldBe` False
        repoInstallationId `shouldBe` 1

itRequiresRepositoryAccess :: RepoP -> YesodSpec App ()
itRequiresRepositoryAccess path = do
  it "404s for unknown repos" $ do
    get $ repoP "foo" "bar" path

    statusIs 404

  it "200s for public repos" $ graph $ do
    Entity _ Repo {..} <- node @Repo () $ ensure $ not . repoIsPrivate

    lift $ do
      get $ repoP repoOwner repoName path

      statusIs 200

  it "404s for inaccessible repos, so as not to leak existence" $ graph $ do
    Entity _ Repo {..} <- node @Repo () $ ensure repoIsPrivate

    lift $ do
      get $ repoP repoOwner repoName path

      statusIs 404

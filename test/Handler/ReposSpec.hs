{-# LANGUAGE OverloadedStrings #-}

module Handler.ReposSpec
    ( spec
    )
where

import TestImport

import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH

spec :: Spec
spec = withApp $ do
    describe "GET gh/:owner/repos" $ do
        it "404s if we have no repos for that owner" $ do
            get $ reposR "foo"
            statusIs 404

        it "200s if all repos are accessible" $ do
            runDB $ insertMany_
                [ publicRepo "foo" "bar"
                , publicRepo "foo" "bat"
                , makeInaccessible $ publicRepo "baz" "quix"
                ]

            get $ reposR "foo"
            statusIs 200

        it "404s if any repository is not accessible" $ do
            runDB $ insertMany_
                [ publicRepo "foo" "bar"
                , makeInaccessible $ publicRepo "foo" "bat"
                , makeInaccessible $ publicRepo "baz" "quix"
                ]

            get $ OwnerP "foo" $ ReposP ReposR
            statusIs 404

    describe "GET gh/:owner/repos/:repo" $ do
        itRequiresRepositoryAccess RepoR

    describe "GET gh/:owner/repos/:repo/jobs" $ do
        itRequiresRepositoryAccess jobsR

    describe "GET gh/:owner/repos/:repo/pulls/:number" $ do
        itRequiresRepositoryAccess $ pullR 1

    describe "GET gh/:owner/repos/:repo/pulls/:number/jobs" $ do
        itRequiresRepositoryAccess $ pullJobsR 1

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
    owner = "foo"
    name = "bar"

publicRepo :: Name Owner -> Name GH.Repo -> Repo
publicRepo owner name = Repo
    { repoOwner = owner
    , repoName = name
    , repoInstallationId = 1
    , repoIsPrivate = False
    , repoDebugEnabled = False
    }

makeInaccessible :: Repo -> Repo
makeInaccessible repo = repo { repoIsPrivate = True }

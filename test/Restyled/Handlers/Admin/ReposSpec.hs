module Restyled.Handlers.Admin.ReposSpec
    ( spec
    )
where

import Restyled.Test

import qualified Database.Persist as P
import Restyled.RestylerImage

spec :: Spec
spec = withApp $ do
    describe "PATCH /admin/repos/#RepoId" $ do
        it "can update enabled and restylerImage" $ do
            repoId <- runDB $ insert $ buildRepo "pbrisbin" "foo"
            void authenticateAsAdmin

            patchJSON (AdminP $ AdminReposP $ AdminRepoR repoId) $ object
                ["enabled" .= False, "restylerImage" .= betaRestylerJSON]

            statusIs 200
            Just updated <- runDB $ P.get repoId
            repoEnabled updated `shouldBe` False
            repoRestylerImage updated `shouldBe` Just betaRestyler

        it "can unset restylerImage" $ do
            repoId <- runDB $ insert buildRepoBeta
            void authenticateAsAdmin

            patchJSON (AdminP $ AdminReposP $ AdminRepoR repoId)
                $ object ["restylerImage" .= (Nothing :: Maybe Text)]

            statusIs 200
            Just updated <- runDB $ P.get repoId
            repoRestylerImage updated `shouldBe` Nothing

        it "doesn't unset restylerImage if omitted" $ do
            repoId <- runDB $ insert buildRepoBeta
            void authenticateAsAdmin

            patchJSON (AdminP $ AdminReposP $ AdminRepoR repoId)
                $ object ["enabled" .= False]

            statusIs 200
            Just updated <- runDB $ P.get repoId
            repoEnabled updated `shouldBe` False
            repoRestylerImage updated `shouldBe` Just betaRestyler

buildRepoBeta :: Repo
buildRepoBeta = (buildRepo "x" "y") { repoRestylerImage = Just betaRestyler }

betaRestyler :: RestylerImage
betaRestyler = restylerImage "restyled/restyler" $ Just "beta"

betaRestylerJSON :: Text
betaRestylerJSON = "restyled/restyler:beta"

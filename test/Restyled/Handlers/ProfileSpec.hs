module Restyled.Handlers.ProfileSpec
    ( spec
    )
where

import Restyled.Test

import qualified Database.Persist as P
import qualified GitHub.Data as GH
import Restyled.Authorization (authRepoCacheKey)
import Restyled.GitHubOrg

spec :: Spec
spec = withApp $ do
    describe "/profile" $ do
        it "requires authentication" $ do
            get ProfileR `shouldRedirectTo` "/auth/login"

        it "shows user repositories" $ do
            runDB $ do
                void $ insertEntity $ buildRepo "pbrisbin" "foo"
                void $ insertEntity $ buildRepo "pbrisbin" "bar"
            void
                $ authenticateAsWith "me@example.com"
                $ (fieldLens UserGithubUserId ?~ 123)
                . (fieldLens UserGithubUsername ?~ "pbrisbin")

            get ProfileR

            statusIs 200
            htmlAnyContain ".profile-repo" "pbrisbin/foo"
            htmlAnyContain ".profile-repo" "pbrisbin/bar"

        it "shows org repositories" $ do
            runDB $ do
                void $ insertEntity $ buildRepo "freckle" "foo"
                void $ insertEntity $ buildRepo "yesodweb" "bar"
            void
                $ authenticateAsWith "me@example.com"
                $ (fieldLens UserGithubUserId ?~ 123)
                . (fieldLens UserGithubUsername ?~ "pbrisbin")
            cacheGitHubOrgs "pbrisbin" ["freckle", "restyled-io", "yesodweb"]

            get ProfileR

            statusIs 200
            htmlAnyContain ".profile-repo" "freckle/foo"
            htmlAnyContain ".profile-repo" "yesodweb/bar"

        it "supports enable/disable for private repo plans" $ do
            (planId, accountId, repoId) <- runDB $ do
                planId <- insert buildPrivateMarketplacePlan
                (planId, , )
                    <$> insert (buildMarketplaceAccount 123 "pbrisbin" planId)
                    <*> insert (buildPrivateRepo "pbrisbin" "private")
            void
                $ authenticateAsWith "me@example.com"
                $ (fieldLens UserGithubUserId ?~ 123)
                . (fieldLens UserGithubUsername ?~ "pbrisbin")
            cacheCallaboratorCanRead repoId "pbrisbin" True
            cacheGitHubOrgs "pbrisbin" []

            get ProfileR

            statusIs 200
            htmlAnyContain ".profile-repo" "pbrisbin/private"
            htmlAnyContain ".profile-repo" "Enable"

            -- clickOn $ ".profile-repo-" <> toPathPiece repoId <> " .enable"
            post $ RepoP "pbrisbin" "private" $ RepoMarketplaceP
                RepoMarketplaceClaimR
            void followRedirect

            htmlAnyContain ".profile-repo" "pbrisbin/private"
            htmlAnyContain ".profile-repo" "Disable"

            enabled <- runDB $ getBy $ UniqueMarketplaceEnabledRepo
                planId
                accountId
                repoId
            void enabled `shouldBe` Just ()

cacheCallaboratorCanRead
    :: RepoId -> GitHubUserName -> Bool -> YesodExample App ()
cacheCallaboratorCanRead repoId user canRead = do
    Just repo <- runDB $ P.get repoId
    setCache (cacheKey $ authRepoCacheKey repo user) canRead

cacheGitHubOrgs
    :: MonadCache m => GH.Name GH.User -> [GH.Name GH.Organization] -> m ()
cacheGitHubOrgs user = setCache (cacheKey $ githubOrgsCacheKey user)
    . map toGitHubOrg
  where
    toGitHubOrg login = GitHubOrg GH.SimpleOrganization
        { GH.simpleOrganizationId = 99
        , GH.simpleOrganizationLogin = login
        , GH.simpleOrganizationUrl = GH.URL "https://github.com/x"
        , GH.simpleOrganizationAvatarUrl = GH.URL "https://github.com/x"
        }

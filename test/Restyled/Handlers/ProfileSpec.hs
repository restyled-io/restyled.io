module Restyled.Handlers.ProfileSpec
    ( spec
    )
where

import Restyled.Test

import qualified GitHub.Data as GH
import Restyled.Handlers.Profile (GitHubOrg(..))

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

cacheGitHubOrgs
    :: MonadCache m => GH.Name GH.User -> [GH.Name GH.Organization] -> m ()
cacheGitHubOrgs user = setCache cacheKey . map toGitHubOrg
  where
    cacheKey = CacheKey $ "profile.orgs." <> toPathPart user
    toGitHubOrg login = GitHubOrg GH.SimpleOrganization
        { GH.simpleOrganizationId = 99
        , GH.simpleOrganizationLogin = login
        , GH.simpleOrganizationUrl = GH.URL "https://github.com/x"
        , GH.simpleOrganizationAvatarUrl = GH.URL "https://github.com/x"
        }

module Restyled.Handlers.ProfileSpec
  ( spec
  ) where

import Restyled.Test

import qualified Database.Persist as P
import qualified GitHub.Data as GH
import Restyled.Authorization (authRepoCacheKey)
import Restyled.GitHubOrg
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
  describe "/profile" $ do
    it "requires authentication" $ do
      get ProfileR `shouldRedirectTo` "/auth/login"

    it "shows user repositories" $ graph $ do
      void
        $ node @Repo ()
        $ edit
        $ (fieldLens RepoOwner .~ "pbrisbin")
        . (fieldLens RepoName .~ "foo")
      void
        $ node @Repo ()
        $ edit
        $ (fieldLens RepoOwner .~ "pbrisbin")
        . (fieldLens RepoName .~ "bar")
      user <-
        genUser "me@example.com"
          $ (fieldLens UserGithubUserId ?~ 123)
          . (fieldLens UserGithubUsername ?~ "pbrisbin")
      lift $ do
        authenticateAs user

        get ProfileR

        statusIs 200
        htmlAnyContain ".profile-repo" "pbrisbin/foo"
        htmlAnyContain ".profile-repo" "pbrisbin/bar"

    it "shows org repositories" $ graph $ do
      void
        $ node @Repo ()
        $ edit
        $ (fieldLens RepoOwner .~ "freckle")
        . (fieldLens RepoName .~ "foo")
      void
        $ node @Repo ()
        $ edit
        $ (fieldLens RepoOwner .~ "yesodweb")
        . (fieldLens RepoName .~ "bar")
      user <-
        genUser "me@example.com"
          $ (fieldLens UserGithubUserId ?~ 123)
          . (fieldLens UserGithubUsername ?~ "pbrisbin")
      lift $ do
        authenticateAs user
        cacheGitHubOrgs
          "pbrisbin"
          ["freckle", "restyled-io", "yesodweb"]

        get ProfileR

        statusIs 200
        htmlAnyContain ".profile-repo" "freckle/foo"
        htmlAnyContain ".profile-repo" "yesodweb/bar"

    it "supports enable/disable for private repo plans (User)" $ graph $ do
      repo <-
        node @Repo ()
          $ edit
          $ (fieldLens RepoOwner .~ "pbrisbin")
          . (fieldLens RepoName .~ "private")
          . (fieldLens RepoIsPrivate .~ True)
      plan <- node @MarketplacePlan () $ edit $ setPlanLimited 1
      account <- genAccount repo plan setAccountUnexpired
      user <-
        genUser "me@example.com"
          $ (fieldLens UserGithubUserId ?~ 123)
          . (fieldLens UserGithubUsername ?~ "pbrisbin")
      lift $ do
        authenticateAs user
        cacheCallaboratorCanRead (entityKey repo) "pbrisbin" True
        cacheGitHubOrgs "pbrisbin" []

        get ProfileR

        statusIs 200
        htmlAnyContain ".profile-repo" "pbrisbin/private"
        htmlAnyContain ".profile-repo" "Enable"

        -- clickOn $ ".profile-repo-" <> toPathPiece (entityKey repo) <> " .enable"
        post
          $ RepoP "pbrisbin" "private"
          $ RepoMarketplaceP
            RepoMarketplaceClaimR
        followRedirect

        htmlAnyContain ".profile-repo" "pbrisbin/private"
        htmlAnyContain ".profile-repo" "Disable"

        enabled <-
          runDB
            $ getBy
            $ UniqueMarketplaceEnabledRepo
              (entityKey plan)
              (entityKey account)
              (entityKey repo)
        void enabled `shouldBe` Just ()

    it "supports enable/disable for private repo plans (Org)" $ graph $ do
      repo <-
        node @Repo ()
          $ edit
          $ (fieldLens RepoOwner .~ "yesodweb")
          . (fieldLens RepoName .~ "yesod")
          . (fieldLens RepoIsPrivate .~ True)
      plan <- node @MarketplacePlan () $ edit $ setPlanLimited 1
      account <- genAccount repo plan setAccountUnexpired
      user <-
        genUser "me@example.com"
          $ (fieldLens UserGithubUserId ?~ 123)
          . (fieldLens UserGithubUsername ?~ "pbrisbin")
      lift $ do
        authenticateAs user
        cacheCallaboratorCanRead (entityKey repo) "pbrisbin" True
        cacheGitHubOrgs "pbrisbin" ["yesodweb"]

        get ProfileR

        statusIs 200
        htmlAnyContain ".profile-repo" "yesodweb/yesod"
        htmlAnyContain ".profile-repo" "Enable"

        -- clickOn $ ".profile-repo-" <> toPathPiece repoId <> " .enable"
        post
          $ RepoP "yesodweb" "yesod"
          $ RepoMarketplaceP
            RepoMarketplaceClaimR
        followRedirect

        htmlAnyContain ".profile-repo" "yesodweb/yesod"
        htmlAnyContain ".profile-repo" "Disable"

        enabled <-
          runDB
            $ getBy
            $ UniqueMarketplaceEnabledRepo
              (entityKey plan)
              (entityKey account)
              (entityKey repo)
        void enabled `shouldBe` Just ()

cacheCallaboratorCanRead
  :: RepoId -> GitHubUserName -> Bool -> YesodExample App ()
cacheCallaboratorCanRead repoId user canRead = do
  Just repo <- runDB $ P.get repoId
  setCache (cacheKey $ authRepoCacheKey repo user) canRead

cacheGitHubOrgs
  :: MonadCache m => GH.Name GH.User -> [GH.Name GH.Organization] -> m ()
cacheGitHubOrgs user =
  setCache (cacheKey $ githubOrgsCacheKey user)
    . map toGitHubOrg
 where
  toGitHubOrg login =
    GitHubOrg
      GH.SimpleOrganization
        { GH.simpleOrganizationId = 99
        , GH.simpleOrganizationLogin = login
        , GH.simpleOrganizationUrl = GH.URL "https://github.com/x"
        , GH.simpleOrganizationAvatarUrl = GH.URL "https://github.com/x"
        }

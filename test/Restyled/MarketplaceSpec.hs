module Restyled.MarketplaceSpec
  ( spec
  ) where

import Restyled.Test

import Restyled.Marketplace
import Restyled.Test.Graphula
import Restyled.Time

spec :: Spec
spec = withApp $ do
  describe "marketplacePlanAllows" $ do
    it "always allows public repos" $ graph $ do
      repo <- node @Repo () $ ensure $ not . repoIsPrivate
      lift $ runDB $ shouldAllow repo Nothing

    it "allows private repos on the unlimited plan" $ graph $ do
      repo <- node @Repo () $ ensure repoIsPrivate
      plan <- node @MarketplacePlan () $ edit setPlanUnlimited
      void $ genAccount repo plan setAccountUnexpired
      lift $ runDB $ shouldAllow repo $ Just $ entityVal plan

    it "disallows private repos on expired plans" $ graph $ do
      expiredAt <- subtractTime (Days 1) <$> getCurrentTime
      repo <- node @Repo () $ ensure repoIsPrivate
      plan <- node @MarketplacePlan () $ edit setPlanUnlimited
      void $ genAccount repo plan $ setAccountExpired expiredAt
      lift $ runDB $ shouldForbidExpired repo expiredAt

    it "disallows private repos without a plan" $ graph $ do
      repo <- node @Repo () $ ensure repoIsPrivate
      lift $ runDB $ shouldForbid repo MarketplacePlanNotFound

    it "disallows private repos on a public plan" $ graph $ do
      repo <- node @Repo () $ ensure repoIsPrivate
      plan <- node @MarketplacePlan () $ edit setPlanPublic
      void $ genAccount repo plan setAccountUnexpired
      lift $ runDB $ shouldForbid repo MarketplacePlanPublicOnly

    it "limits repos on a limited plan" $ graph $ do
      let
        genRepo
          :: GraphulaContext m '[Repo]
          => (Repo -> Bool)
          -> m (Entity Repo)
        genRepo p =
          node @Repo ()
            $ edit (fieldLens RepoOwner .~ "example")
            <> ensure p

      privateRepo1 <- genRepo repoIsPrivate
      privateRepo2 <- genRepo repoIsPrivate
      ossRepo <- genRepo $ not . repoIsPrivate
      plan <- node @MarketplacePlan () $ edit $ setPlanLimited 1
      void $ genAccount privateRepo1 plan setAccountUnexpired

      lift $ runDB $ do
        shouldAllow privateRepo1 $ Just $ entityVal plan
        shouldAllow ossRepo Nothing
        shouldForbid privateRepo2 MarketplacePlanMaxRepos
        shouldAllow privateRepo1 $ Just $ entityVal plan
        shouldForbid privateRepo2 MarketplacePlanMaxRepos

shouldAllow
  :: (HasCallStack, MonadIO m)
  => Entity Repo
  -> Maybe MarketplacePlan
  -> SqlPersistT m ()
shouldAllow repo mPlan = do
  allows <- marketplacePlanAllows repo
  allows `shouldBe` MarketplacePlanAllows mPlan

shouldForbid
  :: (HasCallStack, MonadIO m)
  => Entity Repo
  -> MarketplacePlanLimitation
  -> SqlPersistT m ()
shouldForbid repo with = do
  allows <- marketplacePlanAllows repo
  allows `shouldBe` MarketplacePlanForbids with

shouldForbidExpired
  :: (HasCallStack, MonadIO m) => Entity Repo -> UTCTime -> SqlPersistT m ()
shouldForbidExpired repo expiredAt = do
  allows <- marketplacePlanAllows repo

  -- We want to check for the Expired case without actually comparing the
  -- expiredAt value, since we might fail on ms differences. In the unexpected
  -- case, we only use shouldBe with that value to provide an informative
  -- message.
  case allows of
    MarketplacePlanForbids (MarketplacePlanAccountExpired _) -> pure ()
    x -> x `shouldBe` expected
 where
  expected = MarketplacePlanForbids $ MarketplacePlanAccountExpired expiredAt

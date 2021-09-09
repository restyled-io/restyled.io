module Restyled.OffersSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.Text as T
import qualified Database.Persist as Persist
import Database.Persist.Sql (toSqlKey)
import Restyled.Offers hiding (claimOffer)
import qualified Restyled.Offers as Offers
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
    describe "claimOffer" $ do
        it "requires a valid offer" $ runDB $ do
            result <- claimOffer (toSqlKey 999) "login" "code"

            result `shouldSatisfy` either
                ("does not exist" `T.isInfixOf`)
                (const False)

        it "doesn't allow claiming if you already have an account" $ graph $ do
            plan <- node @MarketplacePlan () mempty
            void $ node @MarketplaceAccount ("login", entityKey plan) mempty
            offer <- node @Offer (onlyKey plan) mempty

            result <- lift $ runDB $ claimOffer
                (entityKey offer)
                "login"
                "some code"

            result `shouldSatisfy` either
                ("Account already exists" `T.isInfixOf`)
                (const False)

        it "rejects invalid codes" $ graph $ do
            plan <- node @MarketplacePlan () mempty
            offer <- node @Offer (onlyKey plan) mempty
            for_ ["a", "b"] $ \code ->
                node @OfferClaim (onlyKey offer)
                    $ edit
                    $ (fieldLens OfferClaimCode .~ code)
                    . (fieldLens OfferClaimClaimedAt .~ Nothing)
                    . (fieldLens OfferClaimClaimedFor .~ Nothing)

            result <- lift $ runDB $ claimOffer (entityKey offer) "login" "c"

            result `shouldSatisfy` either
                ("Invalid Code" `T.isInfixOf`)
                (const False)


        it "claims an offer by code" $ graph $ do
            plan <- node @MarketplacePlan () mempty
            offer <- node @Offer (onlyKey plan) mempty
            for_ ["a", "b"] $ \code ->
                node @OfferClaim (onlyKey offer)
                    $ edit
                    $ (fieldLens OfferClaimCode .~ code)
                    . (fieldLens OfferClaimClaimedAt .~ Nothing)
                    . (fieldLens OfferClaimClaimedFor .~ Nothing)

            (Just account, Just (Entity _ claimA), Just (Entity _ claimB)) <-
                lift $ runDB $ do
                    Right accountId <- claimOffer (entityKey offer) "login" "a"
                    (,,)
                        <$> Persist.get accountId
                        <*> selectFirst
                                [ OfferClaimOffer ==. entityKey offer
                                , OfferClaimCode ==. "a"
                                ]
                                []
                        <*> selectFirst
                                [ OfferClaimOffer ==. entityKey offer
                                , OfferClaimCode ==. "b"
                                ]
                                []

            marketplaceAccountGithubLogin account `shouldBe` "login"
            offerClaimClaimedAt claimA `shouldSatisfy` isJust
            offerClaimClaimedFor claimA `shouldBe` Just "login"
            offerClaimClaimedAt claimB `shouldBe` Nothing
            offerClaimClaimedFor claimB `shouldBe` Nothing

        it "can't claim an offer twice" $ graph $ do
            plan <- node @MarketplacePlan () mempty
            offer <- node @Offer (onlyKey plan) mempty
            void
                $ node @OfferClaim (onlyKey offer)
                $ edit
                $ (fieldLens OfferClaimCode .~ "a")
                . (fieldLens OfferClaimClaimedAt .~ Nothing)
                . (fieldLens OfferClaimClaimedFor .~ Nothing)
            result <- lift $ runDB $ do
                Right accountId <- claimOffer (entityKey offer) "login" "a"
                -- We have to work around the account check to trigger this
                -- condition, which is only possible in a data race
                Persist.delete accountId

                claimOffer (entityKey offer) "login" "a"

            result `shouldSatisfy` either
                ("already claimed" `T.isInfixOf`)
                (const False)

claimOffer
    :: MonadIO m
    => OfferId
    -> GitHubUserName
    -> Text
    -> SqlPersistT m (Either Text MarketplaceAccountId)
claimOffer offerId login code = runExceptT $ Offers.claimOffer
    offerId
    ClaimDetails
        { cdInstallationLogin = login
        , cdEmail = Nothing
        , cdCode = code
        }

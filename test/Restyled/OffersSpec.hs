module Restyled.OffersSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.Text as T
import qualified Database.Persist as Persist
import Database.Persist.Sql (toSqlKey)
import Restyled.Offers hiding (claimOffer)
import qualified Restyled.Offers as Offers

spec :: Spec
spec = withApp $ do
    describe "claimOffer" $ do
        it "requires a valid offer" $ runDB $ do
            result <- claimOffer (toSqlKey 999) "login" "code"

            result `shouldSatisfy` either
                ("does not exist" `T.isInfixOf`)
                (const False)

        it "doesn't allow claiming if you already have an account" $ runDB $ do
            planId <- insert buildPrivateMarketplacePlan
            offerId <- buildOffer planId []
            insert_ $ buildMarketplaceAccount Nothing "login" planId

            result <- claimOffer offerId "login" "code"

            result `shouldSatisfy` either
                ("Account already exists" `T.isInfixOf`)
                (const False)

        it "rejects invalid codes" $ runDB $ do
            planId <- insert buildPrivateMarketplacePlan
            offerId <- buildOffer planId ["a", "b"]

            result <- claimOffer offerId "login" "c"

            result `shouldSatisfy` either
                ("Invalid Code" `T.isInfixOf`)
                (const False)

        it "claims an offer by code" $ runDB $ do
            planId <- insert buildPrivateMarketplacePlan
            offerId <- buildOffer planId ["a", "b"]

            Right accountId <- claimOffer offerId "login" "a"

            Just account <- Persist.get accountId
            Just (Entity _ claimA) <- selectFirst
                [OfferClaimOffer ==. offerId, OfferClaimCode ==. "a"]
                []
            Just (Entity _ claimB) <- selectFirst
                [OfferClaimOffer ==. offerId, OfferClaimCode ==. "b"]
                []
            marketplaceAccountGithubLogin account `shouldBe` "login"
            offerClaimClaimedAt claimA `shouldSatisfy` isJust
            offerClaimClaimedFor claimA `shouldBe` Just "login"
            offerClaimClaimedAt claimB `shouldBe` Nothing
            offerClaimClaimedFor claimB `shouldBe` Nothing

        it "can't claim an offer twice" $ runDB $ do
            planId <- insert buildPrivateMarketplacePlan
            offerId <- buildOffer planId ["a"]
            Right accountId <- claimOffer offerId "login" "a"
            -- We have to work around the account check to trigger this
            -- condition, which is only possible in a data race
            Persist.delete accountId

            result <- claimOffer offerId "login" "a"

            result `shouldSatisfy` either
                ("already claimed" `T.isInfixOf`)
                (const False)

buildOffer :: MonadIO m => MarketplacePlanId -> [Text] -> SqlPersistT m OfferId
buildOffer planId codes = do
    now <- liftIO getCurrentTime
    offerId <- insert Offer
        { offerName = "Test"
        , offerDetails = ""
        , offerPurchaseUrl = ""
        , offerMarketplacePlan = planId
        }
    insertMany_ $ do
        code <- codes
        pure OfferClaim
            { offerClaimOffer = offerId
            , offerClaimCode = code
            , offerClaimCreatedAt = now
            , offerClaimClaimedAt = Nothing
            , offerClaimClaimedFor = Nothing
            }

    pure offerId

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

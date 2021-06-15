module Restyled.Offers
    (
    -- * Creation
      CreateOffer(..)
    , createOffer
    , addClaimCodes

    -- * Claiming
    , ClaimDetails(..)
    , claimOffer
    ) where

import Restyled.Prelude

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Data.List ((!!))
import Database.Persist.Sql (updateWhereCount)
import Restyled.Models
import Restyled.PrivateRepoAllowance
import System.Random

data CreateOffer = CreateOffer
    { coName :: Text
    , coDetails :: Text
    , coPurchaseUrl :: Text
    , coClaims :: Int
    , coPlanId :: Maybe MarketplacePlanId
    , coPrivateRepos :: Maybe Int
    }

createOffer :: MonadIO m => CreateOffer -> SqlPersistT m ()
createOffer CreateOffer {..} = do
    planId <- case (coPlanId, coPrivateRepos) of
        (Nothing, Nothing) -> undefined
        (Just planId, _) -> pure planId
        (_, Just repos) -> insert MarketplacePlan
            { marketplacePlanGithubId = Nothing
            , marketplacePlanName = coName <> " Plan"
            , marketplacePlanDescription = ""
            , marketplacePlanPrivateRepoAllowance = fromNumeric repos
            }
    offerId <- insert Offer
        { offerName = coName
        , offerDetails = coDetails
        , offerPurchaseUrl = coPurchaseUrl
        , offerMarketplacePlan = planId
        }

    addClaimCodes offerId coClaims

addClaimCodes :: MonadIO m => OfferId -> Int -> SqlPersistT m ()
addClaimCodes offerId n = do
    (now, codes) <- liftIO $ (,) <$> getCurrentTime <*> generateClaimCodes n

    insertMany_ $ do
        code <- codes
        pure OfferClaim
            { offerClaimOffer = offerId
            , offerClaimCode = code
            , offerClaimCreatedAt = now
            , offerClaimClaimedAt = Nothing
            , offerClaimClaimedFor = Nothing
            }

generateClaimCodes :: Int -> IO [Text]
generateClaimCodes n = do
    g <- newStdGen
    pure $ pureGenerateClaimCodes g n

pureGenerateClaimCodes :: StdGen -> Int -> [Text]
pureGenerateClaimCodes g n = evalState (replicateM n $ pack <$> pickChars) g
  where
    pickChars :: State StdGen String
    pickChars = replicateM 8 pickChar

    pickChar :: State StdGen Char
    pickChar = do
        (idx, g') <- State.gets $ randomR (0, length chars - 1)
        chars !! idx <$ State.put g'

    chars :: String
    chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

data ClaimDetails = ClaimDetails
    { cdInstallationLogin :: GitHubUserName
    , cdEmail :: Maybe Text
    , cdCode :: Text
    }

claimOffer
    :: MonadIO m
    => OfferId
    -> ClaimDetails
    -> ExceptT Text (SqlPersistT m) MarketplaceAccountId
claimOffer offerId ClaimDetails {..} = do
    mExisting <- lift $ runMaybeT $ fetchMarketplaceAccountForLoginT
        cdInstallationLogin

    when (isJust mExisting)
        $ throwError
        $ "Marketplace Account already exists for Login "
        <> toPathPart cdInstallationLogin

    Offer {..} <- noteT "Offer does not exist" $ getT offerId
    now <- liftIO getCurrentTime
    n <- lift $ updateWhereCount
        [ OfferClaimOffer ==. offerId
        , OfferClaimCode ==. cdCode
        , OfferClaimClaimedAt ==. Nothing
        ]
        [ OfferClaimClaimedAt =. Just now
        , OfferClaimClaimedFor =. Just cdInstallationLogin
        ]

    when (n /= 1) $ throwError "Invalid Code or already claimed"

    -- TODO: Implement offer expiry and match it here
    lift $ insert MarketplaceAccount
        { marketplaceAccountGithubId = Nothing
        , marketplaceAccountGithubLogin = cdInstallationLogin
        , marketplaceAccountGithubType = "User" -- unimportant
        , marketplaceAccountEmail = cdEmail
        , marketplaceAccountBillingEmail = cdEmail
        , marketplaceAccountMarketplacePlan = offerMarketplacePlan
        , marketplaceAccountExpiresAt = Nothing
        }

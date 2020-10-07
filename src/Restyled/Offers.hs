module Restyled.Offers
    (
    -- * Creation
      CreateOffer(..)
    , createOffer
    )
where

import Restyled.Prelude

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Data.List ((!!))
import Restyled.Models
import Restyled.PrivateRepoAllowance
import System.Random

data CreateOffer = CreateOffer
    { coName :: Text
    , coDetails :: Text
    , coPurchaseUrl :: Text
    , coPrivateRepos :: Int
    , coClaims :: Int
    }

createOffer :: MonadIO m => CreateOffer -> SqlPersistT m ()
createOffer CreateOffer {..} = do
    planId <- insert MarketplacePlan
        { marketplacePlanGithubId = Nothing
        , marketplacePlanName = coName <> " Plan"
        , marketplacePlanDescription = ""
        , marketplacePlanPrivateRepoAllowance = fromNumeric coPrivateRepos
        }

    offerId <- insert Offer
        { offerName = coName
        , offerDetails = coDetails
        , offerPurchaseUrl = coPurchaseUrl
        , offerMarketplacePlan = planId
        }

    (now, codes) <-
        liftIO $ (,) <$> getCurrentTime <*> generateClaimCodes coClaims

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

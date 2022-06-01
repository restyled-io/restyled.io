{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

-- | Entities defined via @config/models@
module Restyled.Models.DB
    ( module Restyled.Models.DB
    ) where

import Restyled.Prelude

import qualified Data.Csv as Csv
import Data.Time.ISO8601 (formatISO8601)
import qualified Data.Vector as V
import Database.Persist.Quasi
import Database.Persist.Sql (toSqlKey)
import Database.Persist.TH
import Graphula
import Restyled.PrivateRepoAllowance
import Restyled.RestylerImage
import Restyled.UsCents
import Test.QuickCheck (Positive(..))
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

instance Arbitrary User where
    arbitrary = genericArbitrary

instance HasDependencies User where
    type Dependencies User = ()

instance Arbitrary Repo where
    arbitrary = genericArbitrary

instance HasDependencies Repo where
    type Dependencies Repo = ()

instance Arbitrary JobId where
    arbitrary = toSqlKey . getPositive <$> arbitrary

instance Arbitrary Job where
    arbitrary = genericArbitrary

instance HasDependencies Job where
    type Dependencies Job = (OwnerName, RepoName, PullRequestNum)

instance Arbitrary MarketplacePlanId where
    arbitrary = toSqlKey . getPositive <$> arbitrary

instance Arbitrary MarketplacePlan where
    arbitrary = genericArbitrary

instance HasDependencies MarketplacePlan

instance Arbitrary MarketplaceAccount where
    arbitrary = genericArbitrary

instance HasDependencies MarketplaceAccount where
    type Dependencies MarketplaceAccount = (GitHubUserName, MarketplacePlanId)

instance Arbitrary OfferId where
    arbitrary = toSqlKey . getPositive <$> arbitrary

instance Arbitrary Offer where
    arbitrary = genericArbitrary

instance HasDependencies Offer where
    type Dependencies Offer = Only MarketplacePlanId

instance Arbitrary OfferClaim where
    arbitrary = genericArbitrary

instance HasDependencies OfferClaim where
    type Dependencies OfferClaim = Only OfferId

newtype UTCTimeCsv = UTCTimeCsv UTCTime

instance Csv.ToField UTCTimeCsv where
    toField (UTCTimeCsv t) = Csv.toField $ formatISO8601 t

instance Csv.DefaultOrdered (Entity OfferClaim) where
    headerOrder _ =
        V.fromList ["id", "code", "createdAt", "claimedAt", "claimedFor"]

instance Csv.ToNamedRecord (Entity OfferClaim) where
    toNamedRecord (Entity claimId OfferClaim {..}) = Csv.namedRecord
        [ "id" Csv..= toPathPiece claimId
        , "code" Csv..= offerClaimCode
        , "createdAt" Csv..= UTCTimeCsv offerClaimCreatedAt
        , "claimedAt" Csv..= (UTCTimeCsv <$> offerClaimClaimedAt)
        , "claimedFor" Csv..= offerClaimClaimedFor
        ]

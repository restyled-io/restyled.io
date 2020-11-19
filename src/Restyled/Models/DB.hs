{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

-- | Entities defined via @config/models@
module Restyled.Models.DB
    ( module Restyled.Models.DB
    )
where

import Restyled.Prelude

import qualified Data.Csv as Csv
import Data.Time.ISO8601 (formatISO8601)
import qualified Data.Vector as V
import Database.Persist.Quasi
import Database.Persist.TH
import Restyled.PrivateRepoAllowance
import Restyled.RestylerImage

mkPersist sqlSettings $(persistFileWith lowerCaseSettings "config/models")

instance Display (Entity User) where
    display (Entity userId User {..}) =
        "User #"
            <> display (toPathPiece userId)
            <> " "
            <> maybe "<GitHub unknown>" display userGithubUsername

instance Display (Entity Repo) where
    display (Entity repoId Repo {..}) =
        "Repo #"
            <> display (toPathPiece repoId)
            <> " "
            <> display repoOwner
            <> "/"
            <> display repoName

instance Display (Entity Job) where
    display (Entity jobId Job {..}) =
        "Job #"
            <> display (toPathPiece jobId)
            <> " for "
            <> display jobOwner
            <> "/"
            <> display jobRepo
            <> "#"
            <> display jobPullRequest

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

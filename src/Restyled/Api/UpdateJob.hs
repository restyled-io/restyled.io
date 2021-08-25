module Restyled.Api.UpdateJob
    ( ApiUpdateJob(Complete)
    , ApiCompleteJob(..)
    , ApiUpdateJobErrors
    , ApiUpdateJobError
    , updatedJobNotFound
    , updateJob
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import qualified Data.List.NonEmpty as NE
import Restyled.Api.Job
import Restyled.Models.DB
import Restyled.Settings

data ApiUpdateJob
    = Complete ApiCompleteJob
    | Unused1 -- Trick Aeson into tagging
    deriving stock Generic
    deriving anyclass FromJSON

data ApiCompleteJob = ApiCompleteJob
    { completedAt :: UTCTime
    , exitCode :: Int
    }
    deriving stock Generic
    deriving anyclass FromJSON

newtype ApiUpdateJobErrors = ApiUpdateJobErrors
    { errors :: NonEmpty ApiUpdateJobError
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype Semigroup
    deriving anyclass ToJSON

data ApiUpdateJobError
    = UpdatedJobNotFound JobId
    | Unused2 -- Trick Aeson into tagging
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

updatedJobNotFound :: JobId -> ApiUpdateJobErrors
updatedJobNotFound = ApiUpdateJobErrors . pure . UpdatedJobNotFound

updateJob
    :: (MonadIO m, MonadReader env m, HasSettings env)
    => JobId
    -> NonEmpty ApiUpdateJob
    -> ValidateT ApiUpdateJobErrors (SqlPersistT m) ApiJob
updateJob jobId changes = do
    settings <- lift $ lift $ view settingsL
    mUpdated <- lift $ do
        now <- liftIO getCurrentTime
        update jobId $ updates now
        getEntity jobId
    maybe
        (refute $ updatedJobNotFound jobId)
        (\job -> pure $ apiJob job settings)
        mUpdated
  where
    updates now = concatMap (toUpdates now) $ NE.toList changes

    toUpdates now = \case
        Complete ApiCompleteJob {..} ->
            [ JobUpdatedAt =. now
            , JobCompletedAt =. Just completedAt
            , JobExitCode =. Just exitCode
            ]

        Unused1 -> []

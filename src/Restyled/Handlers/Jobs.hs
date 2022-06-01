module Restyled.Handlers.Jobs
    ( postJobsR
    , patchJobR
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.CreateJob
import Restyled.Api.UpdateJob
import Restyled.DB
import Restyled.Foundation
import Restyled.Models.DB
import Restyled.Yesod

-- | Create a job
--
-- - Request: 'ApiCreateJob'
-- - Response: 400 with 'ApiCreateRepoErrors' or 201 with 'ApiJob'
--
postJobsR :: Handler Value
postJobsR = do
    body <- requireJsonBody
    result <- runDB $ runValidateT $ createJob body
    either (sendStatusJSON status400) (sendStatusJSON status201) result

-- | Update a job
--
-- - Request: 'ApiUpdateJob'
-- - Response: 400 with 'ApiUpdateJobErrors' or 200 with 'ApiJob'
--
patchJobR :: JobId -> Handler Value
patchJobR jobId = do
    void $ runDB $ get404 jobId
    body <- requireJsonBody
    result <- runDB $ runValidateT $ updateJob jobId body
    either (sendStatusJSON status400) (sendStatusJSON status200) result

module Restyled.Handlers.Jobs
    ( postJobsR
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.CreateJob
import Restyled.Foundation
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

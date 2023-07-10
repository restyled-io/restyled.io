module Restyled.Handlers.Repos
  ( getRepoR
  , putRepoR
  ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.UpsertRepo
import Restyled.DB
import Restyled.Foundation
import Restyled.Handlers.Repos.Jobs
import Restyled.Yesod

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

-- | Find or create a repository
--
-- - Request: 'ApiUpsertRepo'
-- - Response: 400 with 'ApiUpsertRepoErrors' or 200 with 'ApiRepo'
putRepoR :: OwnerName -> RepoName -> Handler Value
putRepoR owner name = do
  body <- requireJsonBody
  result <- runDB $ runValidateT $ do
    assertOwnerName owner body *> assertRepoName name body
    upsertRepo body
  either (sendStatusJSON status400) (sendStatusJSON status200) result
